def dig_pow(n, p):
    digits = [int(d) for d in str(n)]
    power_sum = sum([digits[i] ** (p+i) for i in range(len(digits))])
    if power_sum % n == 0:
        return power_sum // n
    else:
        return -1

print(dig_pow(46288, 3))