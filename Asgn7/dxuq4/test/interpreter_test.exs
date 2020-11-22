defmodule InterpreterTest do
  use ExUnit.Case
  doctest Dxuq4

  alias Dxuq4.{NumC, IdC, Binding, NumV}

  test "test interp with NumC" do
    expr = %NumC{val: 127}
    assert Dxuq4.interp(expr, []) == 127
  end

  # test "test interp with IdC" do
  #   expr = %IdC{id: :var}
  #   assert Dxuq4.interp(expr) == :var
  # end

  test "test interp with IdC" do
    env = [%Binding{id: :var, val:  %NumV{val: 10}}]
    expr1 = %IdC{id: :var}
    expr2 = %IdC{id: :x}
    assert Dxuq4.interp(expr1, env) == %NumV{val: 10}
    assert_raise(RuntimeError, "DXUQ unbound identifier", fn() -> Dxuq4.interp(expr2, []) end)
  end

  test "test environment lookup" do
    env = [
      %Binding{id: :x, val: %NumV{val: 5}},
      %Binding{id: :var, val:  %NumV{val: 10}}]
    assert Dxuq4.lookup(:var, env).val == 10
    assert Dxuq4.lookup(:x, env).val == 5
    assert_raise(RuntimeError, "DXUQ unbound identifier", fn() -> Dxuq4.lookup(:y, env) end)
  end

end
