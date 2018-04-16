/*
 * This file is part of Scammander, licensed under the MIT License (MIT).
 *
 * Copyright (c) 2018 Katrix
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
package net.katsstuff.scammander

import scala.language.higherKinds

import cats.data.StateT

trait OrParameters[F[_], RootSender, RunExtra, TabExtra] {
  self: ScammanderBase[F, RootSender, RunExtra, TabExtra] =>

  /**
    * A class which can parse a normal parameter, or can optionally be filled
    * by some other context.
    * @param value The parsed value.
    * @tparam Base The type to parse.
    * @tparam Context The context type which specifies how to parse the value
    *                 if it's not present.
    */
  case class Or[Base, Context](value: Base)

  /**
    * Used in [[Or]]. Parse a value, or return the sender as that value.
    * Requires that a [[UserValidator]] is present for that type.
    */
  sealed trait Source
  type OrSource[Base] = Base Or Source

  implicit def orSource[Base](
      implicit parameter: Parameter[Base],
      validator: UserValidator[Base]
  ): Parameter[OrSource[Base]] =
    new ProxyParameter[OrSource[Base], Base] {
      override def param: Parameter[Base] = parameter

      override def parse(source: RootSender, extra: RunExtra): StateT[F, List[RawCmdArg], OrSource[Base]] =
        for {
          xs <- ScammanderHelper.getArgs[F]
          res <- {
            val fa1      = param.parse(source, extra).run(xs)
            lazy val fa2 = F.map(validator.validate(source))(xs -> _)

            val res = F.handleErrorWith(fa1) { e1 =>
              F.handleErrorWith(fa2) { e2 =>
                F.raiseError(e1 ::: e2)
              }
            }

            StateT.liftF[F, List[RawCmdArg], (List[RawCmdArg], Base)](res).transform((_, t) => t)
          }
        } yield Or(res)

      override def usage(source: RootSender): F[String] =
        F.handleErrorWith(F.map(validator.validate(source))(_ => s"[$name]"))(_ => super.usage(source))
    }
}
