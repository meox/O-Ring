defmodule ExRing do
  @moduledoc """
  Elixir implementation of Ring Exercise
  """

  @spec start({integer(), integer()}) :: {integer, integer}
  def start({n, m}) do
    {creation_time, ring} = :timer.tc(__MODULE__, :create_ring, [n])
    {run_time, 0} = :timer.tc(__MODULE__, :run, [ring, m])
    {creation_time, run_time}
  end

  def run(_ring, 0), do: 0

  def run(ring, step) do
    send(ring, 0)

    receive do
      _ -> run(ring, step - 1)
    end
  end

  @spec create_ring(number) :: pid
  def create_ring(n), do: chain(self(), n)

  # ******************* HELPERS *******************

  @spec chain(pid, number) :: pid
  defp chain(parent, 0), do: parent

  defp chain(parent, n) do
    parent
    |> node_spawn()
    |> chain(n - 1)
  end

  @spec node_spawn(pid) :: pid
  defp node_spawn(dst) do
    spawn(__MODULE__, :process_node, [dst])
  end

  # process node function:
  # take a message and send it to destination node
  def process_node(dst) do
    receive do
      msg ->
        send(dst, msg + 1)
    end

    process_node(dst)
  end
end
