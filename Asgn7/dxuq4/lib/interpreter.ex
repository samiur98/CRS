defmodule Dxuq4 do
  @moduledoc """
  Documentation for `Dxuq4`.
  """

  alias Dxuq4.{NumC, IdC, Binding, NumV}

  @type exprC :: NumC.t() | IdC.t()
  @type environment :: list(Binding.t)
  @type value :: NumV.t

  defmodule NumC do
    defstruct val:
    @type t :: %NumC{val: float}
  end

  defmodule IdC do
    defstruct id:
    @type t :: %IdC{id: atom}
  end

  defmodule Binding do
    defstruct [:id, :val]
    @type t :: %Binding{id: atom, val: Dxuq4.value}
  end

  defmodule NumV do
    defstruct val:
    @type t :: %NumV{val: float}
  end


  @doc """
  Interp
  """
  @spec interp(exprC, environment) :: value
  def interp(expr, env) do
    case expr do

      %{val: value} ->
        value

      %{id: id} ->
        lookup(id, env)

      _ ->
        nil

    end
  end

  #-----------------------------------------------------------
  #Environment Functions

  @spec lookup(atom, environment) :: value
  def lookup(_id, env) when env == [] do
    raise "DXUQ unbound identifier"
  end

  @spec lookup(atom, environment) :: value
  def lookup(id, env) do
    [head | tail] = env
    cond do
      head.id == id ->
        head.val
      true ->
        lookup(id, tail)
    end
  end

  @spec extendEnv(atom, value, environment) :: environment
  def extendEnv(id, val, env) do
    [%Binding{id: id, val: val}] ++ env
  end

  @spec extendAllEnv(list(atom), list(value), environment) :: environment
  def extendAllEnv(ids, _args, env) when ids == [] do
    env
  end

  @spec extendAllEnv(list(atom), list(value), environment) :: environment
  def extendAllEnv(ids, args, env) do
    [firstId | restIds] = ids
    [firstArg | restArgs] = args
    extendAllEnv(restIds, restArgs, extendEnv(firstId, firstArg, env))
  end
end
