00010 ! replace Test\PayTransaction
00020   library 'S:\Core\Library': fnpaytransaction,fntop,fnxit
00030   fntop("Test\PayTransaction","Test Pay Transaction")
00050   fnpaytransaction(10)
00060   fnxit
