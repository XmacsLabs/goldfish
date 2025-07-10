import time

start_time = time.time()

result = [
    [
        x * x 
        for x in range(0, 10000) 
        if (x * x) % 3 == 0
    ]
    for y in range(0, 1000)
]

end_time = time.time()

print("Length of each result:", len(result[0]))
print(f"Elapsed time: {(end_time - start_time) * 1000:.2f} ms")
