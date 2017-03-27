package catz.category

import catz.base.BasePackage._

trait HasTerminalObject[->[_ <: Ξ, _ <: Ξ]] extends Category[->] {
  type Terminal <: Ξ
}
