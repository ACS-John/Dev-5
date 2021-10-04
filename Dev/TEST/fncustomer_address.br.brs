autoLibrary
dim addr$(4)*30
fncustomer_address(z$='100015.04',mat addr$)
pr z$; ' should be addr - has an alt, but it is not used'
pr mat addr$
fncustomer_address(z$='100100.01',mat addr$)
pr z$; ' should be no addr - no such account'
pr mat addr$
fncustomer_address(z$='100065.00',mat addr$)
pr z$; ' should be primary addr'
pr mat addr$
fncustomer_address(z$='100050.04',mat addr$)
pr z$; ' should be alt addr'
pr mat addr$
