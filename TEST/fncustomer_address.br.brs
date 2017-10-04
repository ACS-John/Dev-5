00010   library 'S:\Core\Library': fncustomer_address
00020   dim addr$(4)*30
34000   fncustomer_address(z$='100015.04',mat addr$)
34020   pr z$; ' should be addr - has an alt, but it is not used'
34040   pr mat addr$
48000   fncustomer_address(z$='100100.01',mat addr$)
48020   pr z$; ' should be no addr - no such account'
48040   pr mat addr$
48060   fncustomer_address(z$='100065.00',mat addr$)
48080   pr z$; ' should be primary addr'
48100   pr mat addr$
48120   fncustomer_address(z$='100050.04',mat addr$)
48140   pr z$; ' should be alt addr'
48160   pr mat addr$
