00010 ! replace Test\PayTransaction
00020   library 'S:\Core\Library': fnpaytransaction,fntop,fnxit
00030   let fntop("Test\PayTransaction","Test Pay Transaction")
00050   let fnpaytransaction(10)
00060   let fnxit
