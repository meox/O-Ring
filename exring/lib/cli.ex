defmodule ExRing.CLI do
  def main([n, m]) do
    ExRing.start(
      String.to_integer(n),
      String.to_integer(m)
    )
  end

  def main(_args) do
    IO.puts("./exring N M")
  end
end
