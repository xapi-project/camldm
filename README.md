OCaml bindings to libdevicemapper.
----------------------------------

In a Linux environment, with enough privileges to read device-mapper data
(e.g. where the LVM command "lvs" works):

In utop, load the package:
```ocaml
#require "devmapper";;
```

Query the list of known device-mapper targets:
```ocaml
Devmapper.ls();;
- : bytes list =
["packer--virtualbox--iso--vg-root"; "packer--virtualbox--iso--vg-swap_1"]
```

Query the specific details of one of the targets:
```ocaml
Devmapper.stat "packer--virtualbox--iso--vg-swap_1";;
- : Devmapper.info option =
Some {Devmapper.suspended = false; live_table = 1; inactive_table = 0;
  open_count = 2l; event_nr = 0l; major = 252l; minor = 1l; read_only = false;
  target_count = 1l; deferred_remove = 0;
  targets =
   [{Devmapper.Target.start = 0L; size = 1048576L;
     kind =
      Devmapper.Target.Linear
       {Devmapper.Location.device = Devmapper.Location.Number (8l, 5l);
        offset = 132663296L}}]}
```



