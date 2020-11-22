defmodule Dxuq4Test do
  use ExUnit.Case
  doctest Dxuq4

  test "greets the world" do
    assert Dxuq4.hello() == :world
  end

  test "test NumC" do
    expr = %NumC{}
    assert expr.val == nil
  end
end
