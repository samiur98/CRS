defmodule InterpreterTest do
  use ExUnit.Case
  doctest Dxuq4

  import Dxuq4

  alias Dxuq4.{NumC, IdC, AppC, LamC, IfC, StringC, Binding, NumV, PrimV, BoolV, StringV}

  #-----------------------------------------------------------
  #Top Environment

  @topEnv [
    %{id: :+, val: %PrimV{pfun: &Dxuq4.add/1}},
    %{id: :-, val: %PrimV{pfun: &Dxuq4.subtract/1}},
    %{id: :*, val: %PrimV{pfun: &Dxuq4.multiply/1}},
    %{id: :/, val: %PrimV{pfun: &Dxuq4.divide/1}},
    %{id: :true, val: %BoolV{bool: true}},
    %{id: :false, val: %BoolV{bool: false}}
  ]

  test "test interp with NumC" do
    expr = %NumC{val: 127}
    assert Dxuq4.interp(expr, []) == %NumV{val: 127}
  end

  test "test interp with StringC" do
    expr = %StringC{str: "hello!"}
    assert Dxuq4.interp(expr, []) == %StringV{str: "hello!"}
  end

  test "test interp with IdC" do
    env = [%Binding{id: :var, val:  %NumV{val: 10}}]
    expr1 = %IdC{id: :var}
    expr2 = %IdC{id: :x}
    assert interp(expr1, env) == %NumV{val: 10}
    assert_raise(RuntimeError, "DXUQ unbound identifier", fn() -> interp(expr2, []) end)
  end

  test "test interp with primitives" do
    assert interp(%AppC{fun: %IdC{id: :+}, args: [%NumC{val: 20}, %NumC{val: 10}]}, @topEnv) == %NumV{val: 30}
    assert interp(%AppC{fun: %IdC{id: :-}, args: [%NumC{val: 20}, %NumC{val: 10}]}, @topEnv) == %NumV{val: 10}
    assert interp(%AppC{fun: %IdC{id: :*}, args: [%NumC{val: 20}, %NumC{val: 10}]}, @topEnv) == %NumV{val: 200}
    assert interp(%AppC{fun: %IdC{id: :/}, args: [%NumC{val: 20}, %NumC{val: 10}]}, @topEnv) == %NumV{val: 2}
  end

  test "test interp if statements" do
    assert interp(
      %IfC{test: %IdC{id: :true}, then: %NumC{val: 5}, el: %NumC{val: 10}}, @topEnv
    ) == %NumV{val: 5}
    assert interp(
      %IfC{test: %IdC{id: :false}, then: %NumC{val: 5}, el: %NumC{val: 10}}, @topEnv
    ) == %NumV{val: 10}
  end

  test "test interp with closures" do
    assert interp(
      %AppC{
        fun:
          %LamC{params: [:x], body: %AppC{fun: %IdC{id: :+}, args: [%IdC{id: :x}, %NumC{val: 1}]}},
        args:
          [%NumC{val: 100}]},
    @topEnv) == %NumV{val: 101}
  end

  test "test environment lookup" do
    env = [
      %Binding{id: :x, val: %NumV{val: 5}},
      %Binding{id: :var, val:  %NumV{val: 10}}]
    assert lookup(:var, env).val == 10
    assert lookup(:x, env).val == 5
    assert lookup(:+, @topEnv) == %PrimV{pfun: &add/1}
    assert_raise(RuntimeError, "DXUQ unbound identifier", fn() -> lookup(:y, env) end)
  end

  test "test extendEnv" do
    env = []
    assert_raise(RuntimeError, "DXUQ unbound identifier", fn() -> lookup(:y, env) end)
    env = extendEnv(:y, %NumV{val: 21}, env)
    assert lookup(:y, env).val == 21

  end

  test "test extendAllEnv" do
    env = extendAllEnv([], [], [])
    assert env == []
    env = extendAllEnv([:x, :y], [%NumV{val: 1}, %NumV{val: 2}], [])
    assert lookup(:x, env).val == 1
    assert lookup(:y, env).val == 2
  end

end
