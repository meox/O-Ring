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

  be pass(i: USize) =>
    if i > 0 then
      match _next
      | let n: Ring =>
        n.pass(i - 1)
      end
    else
      _env.out.print(_id.string())
    end

actor Main
  var _ring_size: U32 = 3
  var _ring_count: U32 = 1
  var _trip: USize = 10

  var _env: Env

  new create(env: Env) =>
    _env = env

    try
      parse_args()?
      setup_ring()
    else
      usage()
    end

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
        _trip = value.usize()?
      else
        error
      end
    end

  fun setup_ring() =>
    let first = Ring(1, _env)
    var next = first

    for k in Range[U32](0, _ring_size - 1) do
      let current = Ring(_ring_size - k, _env, next)
      next = current
    end

    first.set(next)

    if _trip > 0 then
      first.pass(_trip)
    end

  fun usage() =>
    _env.out.print(
      """
      ring OPTIONS
        --size N number of actors in each ring
        --trip N number of messages to pass around each ring
      """
      )
