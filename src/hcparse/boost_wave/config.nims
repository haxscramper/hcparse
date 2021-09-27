let p = "cwaveDlPathOverride=" & thisDir() & "/libboost_cwave.so"
echo p
switch("define", p)
switch("define", "debugDlOpen")
