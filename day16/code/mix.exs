defmodule Day16.MixProject do
  use Mix.Project

  def project do
    [
      app: :day16,
      version: "0.1.0",
      elixir: "~> 1.15",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  defp deps do
    [
      {:arrays, "~> 2.1"}
    ]
  end
end
