00040   library 'S:\Core\Library': fntop,fnxit, fnerror,fncno,fnmsgbox,fnopenprn,fncloseprn,fntos,fnlbl,fnacs,fntxt,fncmbcode,fnbutton,fncmdkey,fnfra,fnreceipt,fnopt,fncustomer,fnchk,fnprocess
00050   library 'S:\Core\Library': fnindex_it,fnstatus_close,fnclient_has,fngethandle,fnask_account,fnbutton_or_disabled,fncm_filename$,fnct_filename$,fncd
00052   library 'S:\Core\Library': fnopen_receipt_printer,fnclose_receipt_printer
00060   let fntop(program$,"receipt printer test")
01000   let fnopen_receipt_printer
01020   print #255: 'receipt printer test'
01040   let fnclose_receipt_printer
