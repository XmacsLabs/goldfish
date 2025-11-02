#!/usr/bin/env python3
"""
Python 版本的 rich-vector 性能测试
用于与 Scheme 的 rich-vector 和 vector 性能测试对比
"""

import timeit
import random

def create_random_list(n):
    """创建包含n个随机数的列表"""
    return [random.randint(0, 999) for _ in range(n)]

def list_sum_manual(lst):
    """手动循环求和"""
    total = 0
    for num in lst:
        total += num
    return total

def list_sum_builtin(lst):
    """使用内置sum函数求和"""
    return sum(lst)

def time_operation_with_timeit(stmt, setup, description, number=50000):
    """使用 timeit 库进行性能测试"""
    timer = timeit.Timer(stmt=stmt, setup=setup)
    elapsed = timer.timeit(number=number)
    print(f"{description}:\t{elapsed:.6f} seconds ({number} iterations)")
    return elapsed

if __name__ == "__main__":
    print("Python 数组求和性能测试 (长度=100) - 只计算求和操作")
    print("=" * 60)

    # 预创建数组
    test_list = create_random_list(100)
    print(f"测试数组长度: {len(test_list)}, 总和: {sum(test_list)}")

    # 设置测试环境
    setup_code = f"test_list = {test_list}"

    # 测试手动循环求和
    time_manual = time_operation_with_timeit(
        stmt="list_sum_manual(test_list)",
        setup=f"from __main__ import list_sum_manual; {setup_code}",
        description="list-sum-manual-only"
    )

    # 测试内置sum函数求和
    time_builtin = time_operation_with_timeit(
        stmt="list_sum_builtin(test_list)",
        setup=f"from __main__ import list_sum_builtin; {setup_code}",
        description="list-sum-builtin-only"
    )

    print("\n性能对比:")
    print(f"手动循环 vs 内置sum: {time_manual/time_builtin:.2f}x")

    # 与 Scheme 结果对比（只计算求和操作）
    print("\n与 Scheme 对比 (只计算求和操作，50000次迭代):")
    print(f"Python 手动循环: {time_manual:.6f} 秒")
    print(f"Python 内置sum:  {time_builtin:.6f} 秒")
    print(f"Scheme vector-sum-only: 0.105557 秒 (apply+vector->list)")
    print(f"Scheme rich-vector-sum-only: 0.315977 秒")

    print("\n性能倍数对比:")
    print(f"Python手动循环 / Scheme vector-sum: {time_manual/0.105557:.2f}x")
    print(f"Python内置sum / Scheme vector-sum: {time_builtin/0.105557:.2f}x")
    print(f"Python手动循环 / Scheme rich-vector-sum: {time_manual/0.315977:.2f}x")
    print(f"Python内置sum / Scheme rich-vector-sum: {time_builtin/0.315977:.2f}x")
    print(f"Scheme rich-vector-sum / Scheme vector-sum: {0.315977/0.105557:.2f}x")