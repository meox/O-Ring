defmodule ExRing do
  @moduledoc """
  Elixir implementation of Ring Exercise
  """

  def start(n, m) do
    t0 = Time.utc_now()

    n
    |> create_ring()
    |> run(m)

    t1 = Time.utc_now()
    IO.puts("Total: #{Time.diff(t1, t0, :millisecond)}ms")
  end

  def run(_ring, 0), do: 0
  def run(ring, step) do
    send(ring, step)
    receive do
      msg -> IO.puts(msg)
    end
    run(ring, step - 1)
  end

  @spec create_ring(number) :: pid
  def create_ring(n) do
    t0 = Time.utc_now()
    s = node_up(self())
    ring = chain(s, n - 1)
    t1 = Time.utc_now()
    IO.puts("Ring time: #{Time.diff(t1, t0, :millisecond)}ms")
    ring
  end

  @spec chain(pid, number) :: pid
  defp chain(parent, 0), do: parent
  defp chain(parent, n) do
    parent
    |> node_up()
    |> chain(n - 1)
  end

  defp node_up(dst) do
    spawn(fn -> process_node(dst) end)
  end

  defp process_node(dst) do
    receive do
      msg ->
        send(dst, msg)
    end
    process_node(dst)
  end
end
