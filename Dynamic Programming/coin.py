

def change_possibilities_top_down(amount_left, denominations_left):

    # base cases:
    # we hit the amount spot on. yes!
    if amount_left == 0: return 1
    # we overshot the amount left (used too many coins)
    if amount_left < 0: return 0
    # we're out of denominations
    if len(denominations_left) == 0: return 0

    # print "checking ways to make %i with %s" % (amount_left, denominations_left)

    # choose a current coin
    current_coin, rest_of_coins = denominations_left[0], denominations_left[1:]

    # see how many possibilities we can get
    # for each number of times to use current_coin
    num_possibilities = 0
    while amount_left >= 0:
        num_possibilities += change_possibilities_top_down(amount_left, rest_of_coins)
        amount_left -= current_coin

    return num_possibilities

#print change_possibilities_top_down(4, [1,2,3])

def change_possibilities_bottom_up(amount, denominations):
    ways_of_doing_n_cents = [0] * (amount + 1)
    ways_of_doing_n_cents[0] = 1

    for coin in denominations:
        print "coin %i" % coin
        for higher_amount in xrange(coin, amount + 1):
            higher_amount_remainder = higher_amount - coin
            ways_of_doing_n_cents[higher_amount] += ways_of_doing_n_cents[higher_amount_remainder]
    return ways_of_doing_n_cents[amount]

print change_possibilities_bottom_up(7,[1,2,4])

