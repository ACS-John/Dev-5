dim test$*256
test$='peat'
for x=1 to 10
	test$&='Repeat('&str$(x)&') '
nex x
pr test$