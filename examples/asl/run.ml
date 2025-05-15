
open Main;;

input_stream := stdin;;

if !Sys.interactive then () else begin go(); exit 0 end;;
