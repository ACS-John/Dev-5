20060   library 'R:\Core\Library': fnxit,fncno,fnerror,fntop,fnmsgbox,fnindex_it
20080   on error goto ERTN
30020   dim cap$*128,msgline$(1)*512
30060   let fncno(cno)
30080   let fntop("R:\acsCL\Index",cap$="Recreate All Indexes")
40000   let fnindex_it("Q:\CLmstr\BankMstr.h"&str$(cno), "Q:\CLmstr\BankIdx1.h"&str$(cno),"1 2")
40020   let fnindex_it("Q:\CLmstr\DPTMSTR.h"&str$(cno), "Q:\CLmstr\DPTIDX1.h"&str$(cno),"1 5")
40040   let fnindex_it("Q:\CLmstr\GLmstr.H"&str$(cno), "Q:\CLmstr\GLINDEX.H"&str$(cno),"1 12")
40060   let fnindex_it("Q:\CLmstr\IvPaid.h"&str$(cno), "Q:\CLmstr\IVIndex.h"&str$(cno)," 1 20")
40080   let fnindex_it("Q:\CLmstr\JCBreakdownS"&wsid$&".h"&str$(cno), "Q:\CLmstr\jcbrkidx"&wsid$&".H"&str$(cno),"48 20")
40100   let fnindex_it("Q:\CLmstr\payeeglbreakdown.H"&str$(cno), "Q:\CLmstr\Payeeglbkdidx.H"&str$(cno),"1 8")
40120   let fnindex_it("Q:\CLmstr\paymstr.H"&str$(cno), "Q:\CLmstr\payidx1.H"&str$(cno),"1 8")
40140   let fnindex_it("Q:\CLmstr\PayTrans.h"&str$(cno), "Q:\CLmstr\Unpdidx2.H"&str$(cno),"31/27/1 2/4/26") ! index in year,monthday,reference
40160   let fnindex_it("Q:\CLmstr\PayTrans.h"&str$(cno), "Q:\CLmstr\UNPdIdx1.h"&str$(cno),"1 20")
40180   let fnindex_it("Q:\CLmstr\Receiptglbreakdown.h"&str$(cno), "Q:\CLmstr\receiptglbkdidx.h"&str$(cno),"1 8")
40200   let fnindex_it("Q:\CLmstr\Recmstr.h"&str$(cno), "Q:\CLmstr\Recidx1.h"&str$(cno)," 1 8")
40220   let fnindex_it("Q:\CLmstr\Tralloc.h"&str$(cno), "Q:\CLmstr\Tralloc-idx.h"&str$(cno)," 1 11")
40240   let fnindex_it("Q:\CLmstr\TrMstr.h"&str$(cno), "Q:\CLmstr\TrIdx1.h"&str$(cno)," 1 11")
40260   let fnindex_it("Q:\CLmstr\TrMstr.H"&str$(cno), "Q:\CLmstr\TrIdx2.H"&str$(cno)," 28/1 8/11")
40280   let fnindex_it("Q:\CLmstr\TrMstr.H"&str$(cno), "Q:\CLmstr\Tridx3.H"&str$(cno)," 16/12/4 2/4/8") ! index in year,monthday,reference
40300   let fnindex_it("Q:\CLmstr\unpdaloc.H"&str$(cno), "Q:\CLmstr\Uaidx2.H"&str$(cno)," 1 20")
40320   let fnindex_it("Q:\CLmstr\unpdaloc.H"&str$(cno), "Q:\CLmstr\Uaidx1.H"&str$(cno),"9,12")
40340   let fnindex_it("Q:\CLmstr\unpdaloc.H"&str$(cno), "Q:\CLmstr\Uaidx2.H"&str$(cno),"1,20")
50100 XIT: let fnxit
62120 ! ______________________________________________________________________
62140 ! <Updateable Region: ERTN>
62160 ERTN: let fnerror(cap$,err,line,act$,"xit")
62180   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
62200   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
62220   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
62240 ERTN_EXEC_ACT: execute act$ : goto ERTN
62260 ! /region
62280 ! ______________________________________________________________________
