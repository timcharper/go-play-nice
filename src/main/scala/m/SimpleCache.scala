package m

// dumb. thread safe, but doesn't prevent concurrent generation.
class SimpleCache[K, V](generator: K => V) {
  private var values = Map.empty[K, V]

  def apply(k: K): V =
    values.getOrElse(k, {
      val r = generator(k)
      values = values.updated(k, r)
      r
    })
}
object SimpleCache {
  def apply[K, V](generator: K => V) = new SimpleCache(generator)
}

