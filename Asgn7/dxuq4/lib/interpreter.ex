defmodule Dxuq4 do
  @moduledoc """
  Documentation for `Dxuq4`.
  """

  alias Dxuq4.{NumC, IdC, AppC, Binding, NumV}

  @type exprC :: NumC.t() | IdC.t() | AppC.t()
  @type environment :: list(Binding.t)
  @type value :: NumV.t | PrimV.t

  defmodule NumC do
    defstruct val:
    @type t :: %NumC{val: float}
  end

  defmodule IdC do
    defstruct id:
    @type t :: %IdC{id: atom}
  end

  defmodule AppC do
    defstruct [:fun, :args]
    @type t :: %AppC{fun: Dxuq4.exprC, args: list(Dxuq4.exprC)}
  end

  defmodule Binding do
    defstruct [:id, :val]
    @type t :: %Binding{id: atom, val: Dxuq4.value}
  end

  defmodule NumV do
    defstruct val:
    @type t :: %NumV{val: float}
  end

  defmodule PrimV do
    defstruct pfun:
    @type t :: %PrimV{pfun: (list(Dxuq4.value) -> Dxuq4.value)}
  end


  @doc """
  Interp
  """
  @spec interp(exprC, environment) :: value
  def interp(expr, env) do
    case expr do

      %NumC{val: num} ->
        %NumV{val: num}

      %IdC{id: id} ->
        lookup(id, env)

      %AppC{fun: fun, args: args} ->

        fun_val = interp(fun, env)

        case fun_val do

          %{pfun: pfun} ->

            pfun.(Enum.map(args, fn a -> interp(a, env) end))

          _ ->
            raise "DXUQ invalid function call"

        end

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


  #-----------------------------------------------------------
  #Primitive Functions

  @spec add(list(value)) :: value
  def add(args) do

    case args do

      [%{val: left}, %{val: right}] ->
        %NumV{val: left + right}

      _ ->
        raise "DXUQ invalid args passed to +"
    end
  end



  @spec subtract(list(value)) :: value
  def subtract(args) do

    case args do

      [%{val: left}, %{val: right}] ->
        %NumV{val: left - right}

      _ ->
        raise "DXUQ invalid args passed to -"
    end
  end

  @spec multiply(list(value)) :: value
  def multiply(args) do

    case args do

      [%{val: left}, %{val: right}] ->
        %NumV{val: left * right}

      _ ->
        raise "DXUQ invalid args passed to *"
    end
  end

  @spec divide(list(value)) :: value
  def divide(args) do

    case args do

      [%{val: left}, %{val: right}] ->
        %NumV{val: left / right}

      _ ->
        raise "DXUQ invalid args passed to /"
    end
  end


end
