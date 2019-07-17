package eighties.fiveaday

package object run {
  sealed trait MoveType
  case object Move extends MoveType
  case object RandomMove extends MoveType
  case object NoMove extends MoveType
}
