use "collections"
use "time"

actor Ring
  let _id: U32
  let _env: Env
  var _next: (Ring | None)

  new create(id: U32, env: Env, neighbor: (Ring | None) = None) =>
    _id = id
    _env = env
    _next = neighbor

  be set(neighbor: Ring) =>
    _next = neighbor

  be pass(main: Main, i: U32) =>
    if i > 0 then
      match _next
      | let n: Ring =>
        n.pass(main, i - 1)
      end
    else
      main.complete()
    end

actor Main
  var _ring_size: U32 = 3
  var _trip: U32 = 10
  var _orig_trip: U32 = 10

  var _env: Env
  var _ring: Ring

  var _time_ring: U64 = 0
  var _time_trip: U64 = 0

  new create(env: Env) =>
    _env = env
    _ring = Ring(0, _env)

    try
      parse_args()?
      _orig_trip = _trip

      var t0: U64 = Time.millis()
      _ring = setup_ring()
      var t1: U64 = Time.millis()
      _time_ring = t1 - t0

      _time_trip = Time.millis()
      _trip = _trip - 1
      _ring.pass(this, _ring_size)
    else
      usage()
    end

  be complete() =>
    if _trip > 0 then
      _trip = _trip - 1
      _ring.pass(this, _ring_size)
    else
      finish()
    end

  be finish() =>
    _time_trip = Time.millis() - _time_trip
    _env.out.print(_time_ring.string() + " " + _time_trip.string() + " " + _ring_size.string() + " " + _orig_trip.string())

  fun ref parse_args() ? =>
    _ring_size = _env.args(1)?.u32()?
    _trip = _env.args(2)?.u32()?

  fun setup_ring(): Ring =>
    let first = Ring(1, _env)
    var next = first

    for k in Range[U32](0, _ring_size - 1) do
      let current = Ring(_ring_size - k, _env, next)
      next = current
    end

    first.set(next)
    first

  fun usage() =>
    _env.out.print(
      """
      ring N M
        N number of actors in each ring
        M number of messages to pass around each ring
      """
      )
