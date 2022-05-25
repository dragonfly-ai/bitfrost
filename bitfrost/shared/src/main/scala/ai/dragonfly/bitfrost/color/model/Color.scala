package ai.dragonfly.bitfrost.color.model

trait Color[C] {
  def similarity(that:C):Double
}
