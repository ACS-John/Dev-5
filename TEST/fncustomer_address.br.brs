00010   library 'S:\Core\Library': fncustomer_address
00020   dim addr$(4)*30
34000   let fncustomer_address(z$='100015.04',mat addr$)
34020   print z$; ' should be addr - has an alt, but it is not used'
34040   print mat addr$
48000   let fncustomer_address(z$='100100.01',mat addr$)
48020   print z$; ' should be no addr - no such account'
48040   print mat addr$
48060   let fncustomer_address(z$='100065.00',mat addr$)
48080   print z$; ' should be primary addr'
48100   print mat addr$
48120   let fncustomer_address(z$='100050.04',mat addr$)
48140   print z$; ' should be alt addr'
48160   print mat addr$
