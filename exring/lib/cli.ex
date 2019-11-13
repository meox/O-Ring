defmodule ExRing.CLI do
  def main([n, m]) do
    ExRing.start(
      String.to_integer(n),
      String.to_integer(m)
    ) |> print_times(n, m, :millisecond)
  end

  def main(_args) do
    IO.puts("./exring N M")
  end

  defp print_times({creation_time, run_time}, n, m, :millisecond) do
    IO.puts("#{to_ms_string(creation_time)} #{to_ms_string(run_time)} #{n} #{m}")
  end

  def to_ms_string(time_micro) do
    time_micro/1_000
    |> Float.round()
    |> :erlang.float_to_binary(decimals: 0)
  end
end
