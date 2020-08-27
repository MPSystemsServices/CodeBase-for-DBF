Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX101.VB   
  
    Sub ExCode(ByRef frm As Form1)
        Dim f4memoBinary As Object
        Dim f4binary As Object
        Dim f4memoAssignBinary As Object
        Dim f4assignBinary As Object
        Dim cb As Integer
        cb = code4init()
        Call code4safety(cb, 0)

        Dim fields(2) As FIELD4INFO
        fields(0).fName = "FIELD"
        fields(0).ftype = r4str
        fields(0).flength = 10
        fields(1).fName = "MFIELD"
        fields(1).ftype = r4memo

        Dim field, data, mfield As Integer
        data = d4createData(cb, "bin", fields)
        field = d4field(data, "field")
        mfield = d4field(data, "mfield")

        'UPGRADE_WARNING: Lower bound of array bin was changed from 5 to 0. Click for more: 'ms-help://MS.VSCC/commoner/redir/redirect.htm?keyword="vbup1033"'
        Dim bin(8) As Byte ' prepare a Byte array to append
        bin(5) = &H8S
        bin(6) = &H9S
        bin(7) = &HAS
        bin(8) = &HBS

        Call d4appendStart(data, 0)
        Call f4assignBinary(field, bin)
        Call f4memoAssignBinary(mfield, bin)
        Call d4append(data)

        Dim getdata() As Byte
        'UPGRADE_WARNING: Couldn't resolve default property of object f4binary(). Click for more: 'ms-help://MS.VSCC/commoner/redir/redirect.htm?keyword="vbup1037"'
        getdata = f4binary(field)
        ' getdata contains 4 bytes from the 'bin' array plus 6 spaces
        'UPGRADE_WARNING: Couldn't resolve default property of object f4memoBinary(). Click for more: 'ms-help://MS.VSCC/commoner/redir/redirect.htm?keyword="vbup1037"'
        getdata = f4memoBinary(mfield)
        ' getdata contains 4 bytes from the 'bin' array

        Call code4initUndo(cb)
    End Sub
End Module