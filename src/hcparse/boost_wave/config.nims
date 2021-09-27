let p = "waveDlPathOverride=" & thisDir() & "/libboost_wave.so"
echo p
switch("define", p)
switch("define", "debugDlOpen")
