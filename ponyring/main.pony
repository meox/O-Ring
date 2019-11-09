use "collections"

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

  var _env: Env
  var _ring: Ring

  new create(env: Env) =>
    _env = env
    _ring = Ring(0, _env)

    try
      parse_args()?
      _ring = setup_ring()
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
    _env.out.print("finish")

  fun ref parse_args() ? =>
    var i: USize = 1

    while i < _env.args.size() do
      // Every option has an argument.
      var option = _env.args(i)?
      var value = _env.args(i + 1)?
      i = i + 2

      match option
      | "--size" =>
        _ring_size = value.u32()?
      | "--trip" =>
        _trip = value.u32()?
      else
        error
      end
    end

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
      ring OPTIONS
        --size N number of actors in each ring
        --trip N number of messages to pass around each ring
      """
      )
