defmodule Dxuq4 do
  @moduledoc """
  Documentation for `Dxuq4`.
  """

  alias Dxuq4.{NumC, IdC, AppC, Binding, NumV}

  @type exprC :: NumC.t() | IdC.t() | AppC.t() | LamC.t() | IfC.t() | StringC.t()
  @type environment :: list(Binding.t)
  @type value :: NumV.t | PrimV.t | CloV.t | BoolV.t | StringV.t

  defmodule NumC do
    defstruct val:
    @type t :: %NumC{val: float}
  end

  defmodule IdC do
    defstruct id:
    @type t :: %IdC{id: atom}
  end

  defmodule StringC do
    defstruct str:
    @type t :: %StringC{str: String.t}
  end

  defmodule LamC do
    defstruct [:params, :body]
    @type t :: %LamC{params: list(:atom), body: Dxuq4.exprC}
  end

  defmodule IfC do
    defstruct [:test, :then, :el]
    @type t :: %IfC{test: Dxuq4.exprC, then: Dxuq4.exprC, el: Dxuq4.exprC}
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

  defmodule BoolV do
    defstruct bool:
    @type t :: %BoolV{bool: boolean}
  end

  defmodule StringV do
    defstruct str:
    @type t :: %StringV{str: String.t}
  end

  defmodule PrimV do
    defstruct pfun:
    @type t :: %PrimV{pfun: (list(Dxuq4.value) -> Dxuq4.value)}
  end

  defmodule CloV do
    defstruct [:params, :body, :cloEnv]
    @type t :: %CloV{params: list(atom), body: Dxuq4.exprC, cloEnv: Dxuq4.environment}
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

      %StringC{str: str} ->
        %StringV{str: str}

      %LamC{params: params, body: body} ->
        %CloV{params: params, body: body, cloEnv: env}

      %IfC{test: test, then: then, el: el} ->

        case interp(test, env) do
          %BoolV{bool: true} -> interp(then, env)
          %BoolV{bool: false} -> interp(el, env)
        end

      %AppC{fun: fun, args: args} ->

        #evaluate expression that should return a function value
        fun_val = interp(fun, env)

        case fun_val do

          %PrimV{pfun: pfun} ->
            pfun.(Enum.map(args, fn a -> interp(a, env) end))

          %CloV{params: params, body: body, cloEnv: cloEnv} ->

            #check number of arguments matches number of parameters
            if length(params) != length(args) do
                raise "DXUQ incorrect number of args passed to function"
            end

            #evaluate arguments -- eager
            arg_vals = Enum.map(args, fn a -> interp(a, env) end)

            #extend cloEnv with parameters
            new_env = extendAllEnv(params, arg_vals, cloEnv)

            #evaluate function body with new env
            interp(body, new_env)

          _ ->
            raise "DXUQ invalid function call"
        end
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
