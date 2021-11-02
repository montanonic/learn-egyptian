defmodule LearnEg.Repo do
  use Ecto.Repo,
    otp_app: :learn_eg,
    adapter: Ecto.Adapters.Postgres
end
