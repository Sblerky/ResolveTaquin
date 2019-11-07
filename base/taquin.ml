open Graphics;;
module R = Random;;
module A = Array;;
module U = Unix;;

let taille = 4;;
let div = 700/taille;;

open_graph " 700x700";;
let taquin = A.make_matrix taille taille 0;;
let taquin_init = A.make_matrix taille taille 0;;

let num_case n x y =
    moveto x y;
    set_font "-*-fixed-medium-r-semicondensed--30-*-*-*-*-*-iso8859-1";
    let num = string_of_int n in
    draw_string num;;

(*Remplit les matrices taquin et taquin_init*)
let remp_mat =
    for i=0 to taille-1 do
        for j=0 to taille-1 do
            if i==taille-1 && j==taille-1 then
                begin
                    taquin.(i).(j) <- 0;
                    taquin_init.(i).(j) <- 0;
                end
            else
                begin
                    taquin.(i).(j) <- i*taille+j+1;
                    taquin_init.(i).(j) <- i*taille+j+1;
                end
        done;
    done;;

(*Déplace la case i j, attention à toujours utiliser sur la case vide*)
let deplacer_case i j p =
    let temp = ref taquin.(i).(j) in
    match p with
    | 0 -> begin
        taquin.(i).(j) <- taquin.(i).(j+1);
        taquin.(i).(j+1) <- !temp;  (*droite*)
    end;
    | 1 -> begin
        taquin.(i).(j) <- taquin.(i+1).(j);
        taquin.(i+1).(j) <- !temp   (*bas*)
    end;
    | 2 -> begin
        taquin.(i).(j) <- taquin.(i).(j-1);
        taquin.(i).(j-1) <- !temp      (*gauche*)
    end;
    | 3 -> begin
        taquin.(i).(j) <- taquin.(i-1).(j);
        taquin.(i-1).(j) <- !temp;   (*haut*)
    end;;

(*Remplit l'IHM*)
let remp_graph n =
    clear_graph();
    for i=0 to n-1 do
        for j=0 to n-1 do
            if taquin.(i).(j)==0 then
                begin
                    set_color black;
                    fill_rect (j*div) (600-i*div) div div;
                end
            else
                begin
                    draw_rect (j*div) (600-i*div) div div;
                    num_case taquin.(i).(j) (j*div+10) (600-i*div+10);
                end
        done;
    done;;


(*Mélange le taquin*)
let echange_case n nb =
    let r = ref 0 in
    for k=0 to nb do
        for i=0 to n-1 do
            for j=0 to n-1 do
                if taquin.(i).(j) == 0 then
                    begin
                        if i==0 then
                        begin
                            if j==0 then
                            begin
                                r := (R.int 2);
                                deplacer_case i j !r;
                            end
                            else if j==n-1 then
                            begin
                                r := (R.int 2)+1;
                                deplacer_case i j !r;
                            end
                            else
                            begin
                                r := (R.int 3);
                                deplacer_case i j !r;
                            end
                        end
                        else if i==n-1 then
                        begin
                            if j==0 then
                            begin
                                r := (R.int 2)*3;
                                deplacer_case i j !r;
                            end
                            else if j==n-1 then
                            begin
                                r := (R.int 2)+2;
                                deplacer_case i j !r;
                            end
                            else
                            begin
                                r := (R.int 4);
                                while !r==1 do
                                    r := (R.int 4);
                                done;
                                deplacer_case i j !r;
                            end
                        end
                        else if j==0 then
                        begin
                            r := (R.int 4);
                            while !r==2 do
                                r := (R.int 4);
                            done;
                            deplacer_case i j !r;
                        end
                        else if j==n-1 then
                        begin
                            r := (R.int 3)+1;
                            deplacer_case i j !r;
                        end
                        else
                        begin
                            r := (R.int 4);
                            deplacer_case i j !r;
                        end
                    end
            done;
        done;
    done;
    remp_graph taille;;
(*

let iv = ref 0;;
let jv = ref 0;;
let iz = ref 0;;
let jz = ref 0;;
let iVf = ref 0;;
let jVf = ref 0;;
let iZf = ref 0;;
let jZf = ref 0;;

let search a b =
    for i=0 to taille-1 do
        for j=0 to taille-1 do
            match a with
            | _ when a < taille-2 -> begin

                match b with
                | _ when b < taille-2 -> begin

                    if taquin_init.(a).(b) == taquin.(i).(j) then
                    begin

                        iv := i;
                        jv := j;
                        iVf := !iv - a;
                        jVf := !jv - b;

                    end;

                end;
                | _ when b == taille-2 -> begin

                    if taquin_init.(a).(b) == taquin.(i).(j) then
                    begin

                        if a == i && b == j then begin
                            iv := i;
                            jv := j;
                            iVf := !iv - a;
                            jVf := !jv - b;
                        end
                        else begin
                            iv := i;
                            jv := j;
                            iVf := !iv - a;
                            jVf := !jv - (b+1);
                        end;

                    end;

                end;
                | _ when b == taille-1 -> begin

                    if taquin_init.(a).(b) == taquin.(i).(j) then
                    begin

                        if a == i && b == j then begin
                            iv := i;
                            jv := j;
                            iVf := !iv - a;
                            jVf := !jv - b;
                        end
                        else begin
                            iv := i;
                            jv := j;
                            iVf := !iv - (a+1);
                            jVf := !jv - b;
                        end;

                    end;

                end;

            end;
            | a when a == taille-2 -> begin

                match b with
                | b when b == taille-2 -> begin

                    if taquin_init.(a).(b) == taquin.(i).(j) then begin

                        iv := i;
                        jv := j;
                        iVf := !iv - a;
                        jVf := !jv - b;

                    end;

                end;
                | _ -> begin

                    if taquin_init.(a).(b) == taquin.(i).(j) then begin

                        if a == i && b == j then begin
                            iv := i;
                            jv := j;
                            iVf := !iv - a;
                            jVf := !jv - b;
                        end
                        else begin
                            iv := i;
                            jv := j;
                            iVf := !iv - (a+1);
                            jVf := !jv - b;
                        end;

                    end;

                end;

            end;
            | _ -> begin

                if taquin_init.(a).(b) == taquin.(i).(j) then begin

                    if a == i && b == j then begin
                        iv := i;
                        jv := j;
                        iVf := !iv - a;
                        jVf := !jv - b;
                    end
                    else begin
                        iv := i;
                        jv := j;
                        iVf := !iv - a;
                        jVf := !jv - (b+1);
                    end;

                end;

            end;

        done;

    done;
    for i=0 to taille-1 do
        for j=0 to taille-1 do
            if taquin_init.(taille-1).(taille-1) == taquin.(i).(j) then
                begin
                    iz := i;
                    jz := j;
                    iZf := !iz - !iv;
                    jZf := !jz - !jv;
                end;
        done;
    done;;

let dep_z i j f l m =
    let a = ref i in
    let b = ref j in
    deplacer_case !a !b f;
    remp_graph taille;
    match f with
    | 0 -> begin

        b := !b+1;
        match l with
        | 1 -> begin

            deplacer_case !a !b l;
            remp_graph taille;
            if m == 1 then begin
                a := !a+1; deplacer_case !a !b l; remp_graph taille;
                a := !a+1; deplacer_case !a !b 2; remp_graph taille;
            end;

        end;
        | 3 -> begin

            deplacer_case !a !b l; remp_graph taille;
            if m == 1 then begin
                a := !a-1; deplacer_case !a !b l; remp_graph taille;
                a := !a-1; deplacer_case !a !b 2; remp_graph taille;
            end;

        end

    end;
    | 1 -> begin

        a := !a+1;
        match l with
        | 0 -> begin

            deplacer_case !a !b l; remp_graph taille;
            if m == 1 then begin
                b := !b+1; deplacer_case !a !b l; remp_graph taille;
                b := !b+1; deplacer_case !a !b 3; remp_graph taille;
            end;

        end;
        | 2 -> begin

            deplacer_case !a !b l;
            if m == 1 then begin
                b := !b-1; deplacer_case !a !b l; remp_graph taille;
                b := !b-1; deplacer_case !a !b 3; remp_graph taille;
            end;

        end

    end;
    | 2 -> begin

        b := !b-1;
        match l with
        | 1 -> begin

            deplacer_case !a !b l; remp_graph taille;
            if m == 1 then begin
                a := !a+1; deplacer_case !a !b l; remp_graph taille;
                a := !a+1; deplacer_case !a !b 0; remp_graph taille;
            end;

        end;
        | 3 -> begin

            deplacer_case !a !b l; remp_graph taille;
            if m == 1 then begin
                a := !a-1; deplacer_case !a !b l; remp_graph taille;
                a := !a-1; deplacer_case !a !b 0; remp_graph taille;
            end;

        end

    end;
    | 3 -> begin

        a := !a-1;
        match l with
        | 0 -> begin

            deplacer_case !a !b l; remp_graph taille;
            if m == 1 then begin
                b := !b+1; deplacer_case !a !b l; remp_graph taille;
                b := !b+1; deplacer_case !a !b 1; remp_graph taille;
            end;

        end;
        | 2 -> begin

            deplacer_case !a !b l; remp_graph taille;
            if m == 1 then begin
                b := !b-1; deplacer_case !a !b l; remp_graph taille;
                b := !b-1; deplacer_case !a !b 1; remp_graph taille;
            end;

        end

    end;;

let rec dep_v a b =
    search a b;
    if !iVf<>0 || !jVf<>0 then begin

        match !iVf with
        | 0 -> begin

            match !iZf with
            | 0 -> begin

                match a with
                | _ when a < taille-2 -> begin

                    match b with
                    | _ when b == taille-2 -> deplacer_case !iz !jz 2;
                    | _ when b == taille-1 -> begin

                        match !jZf with
                        | _ when !jZf > 0 -> deplacer_case !iz !jz 2;
                        | _ -> dep_z !iz !jz 1 0 1;

                    end;
                    | _ -> begin

                        match !jZf with
                        | _ when !jZf < 0 -> deplacer_case !iz !jz 0; print_int 1;
                        | _ when !jZf > 1 -> deplacer_case !iz !jz 2;
                        | 1 -> dep_z !iz !jz 1 2 1;

                    end;

                end;
                | _ -> begin

                    match !jZf with
                    | _ when !jZf < 0 -> deplacer_case !iz !jz 0;
                    | _ when !jZf > 1 -> deplacer_case !iz !jz 2;
                    | 1 -> dep_z !iz !jz 3 2 1;

                end;

            end;
            | _ -> begin

                match b with
                | _ when b == taille-2 -> begin

                    match !jZf with
                    | 1 -> deplacer_case !iz !jz 3;
                    | _ -> deplacer_case !iz !jz 0;

                end;
                | _ -> begin

                    match !jZf with
                    | _ when !jZf < -1 -> deplacer_case !iz !jz 0;
                    | _ when !jZf > 1 -> begin

                        deplacer_case !iz !jz 2;
                        dep_v a b;

                    end;
                    | 0 -> deplacer_case !iz !jz 2;
                    | 1 when !iZf < 0 -> begin

                        deplacer_case !iz !jz 1;
                        dep_v a b;

                    end;
                    | -1 when !iZf < 0 -> begin

                        dep_z !iz !jz 1 0 0;
                        dep_v a b;

                    end;
                    | 1 | -1 -> begin

                        deplacer_case !iz !jz 3;
                        dep_v a b;

                    end;

                end;

            end;

        end;
        | _ -> begin

            match !jVf with
            | 0 -> begin

                match !iZf with
                | 0 -> begin

                    match b with
                    | _ when b > taille-3 -> begin

                        match !jZf with
                        | -1 -> dep_z !iz !jz 3 0 0;
                        | _ when a < taille-2 -> deplacer_case !iz !jz 0;
                        | _ -> deplacer_case !iz !jz 3;

                    end;
                    | _ -> begin

                        match !jZf with
                        | _ when !jZf < 0 -> begin

                            if !iz == taille-1 then dep_z !iz !jz 3 0 0
                            else dep_z !iz !jz 1 0 0;

                        end;
                        | _ when !jZf > 0 -> begin

                            match a with
                            | _ when a < taille-2 -> begin

                                if !iz == taille-1 then dep_z !iz !jz 3 2 0
                                else dep_z !iz !jz 1 2 0;

                            end;
                            | _ -> dep_z !iz !jz 1 2 0;

                        end;

                    end;

                end;
                | _ when !iZf < 0 -> begin

                    match !jZf with
                    | _ when !jZf < 0 -> deplacer_case !iz !jz 0;
                    | _ when !jZf > 0 -> deplacer_case !iz !jz 2;
                    | 0 -> deplacer_case !iz !jz 1;

                end;
                | _ when !iZf > 0 -> begin

                    match b with
                    | _ when b > taille-3 -> begin

                        match !jZf with
                        | 0 -> dep_z !iz !jz 2 3 0;
                        | _ -> deplacer_case !iz !jz 3;

                    end;
                    | _ -> begin

                        match a with
                        | _ when a < taille-2 -> begin

                            match !jZf with
                            | _ when !jZf < 0 -> deplacer_case !iz !jz 0;
                            | _ when !jZf > 0 -> deplacer_case !iz !jz 2;
                            | 0 when !iZf > 1 -> deplacer_case !iz !jz 3;
                            | 0 when !iZf == 1 -> dep_z !iz !jz 0 3 1;

                        end;
                        | _ -> begin

                            match !jZf with
                            | 0 -> deplacer_case !iz !jz 3;
                            | _ -> deplacer_case !iz !jz 2;

                        end;

                    end;

                end;

            end;
            | _ when !jVf < 0 -> begin

                match !iZf with
                | 0 -> begin

                    match !jZf with
                    | _ when !jZf < -1 -> deplacer_case !iz !jz 0;
                    | _ when !jZf > 0 -> deplacer_case !iz !jz 2;
                    | -1 -> begin

                        if !iz == taille-1 then dep_z !iz !jz 3 0 1
                        else dep_z !iz !jz 1 0 1;

                    end;

                end;
                | _ when !iZf < 0 -> begin

                    match !jZf with
                    | _ when !jZf <> 0 -> deplacer_case !iz !jz 1;
                    | 0 -> deplacer_case !iz !jz 0;

                end;
                | _ when !iZf > 0 -> begin

                    match !jZf with
                    | _ when !jZf <> 0 -> deplacer_case !iz !jz 3;
                    | 0 -> deplacer_case !iz !jz 0;

                end;

            end;
            | _ when !jVf > 0 -> begin

                match !iZf with
                | 0 -> begin

                    match !jZf with
                    | _ when !jZf < 0 -> deplacer_case !iz !jz 0;
                    | _ when !jZf > 1 -> deplacer_case !iz !jz 2;
                    | 1 -> begin

                        if !iz == taille-1 then dep_z !iz !jz 3 2 1
                        else dep_z !iz !jz 1 2 1;

                    end;

                end;
                | _ when !iZf < 0 -> begin

                    match !jZf with
                    | _ when !jZf <> 0 -> deplacer_case !iz !jz 1;
                    | 0 -> deplacer_case !iz !jz 2;

                end;
                | _ when !iZf > 0 -> begin

                    match !jZf with
                    | _ when !jZf <> 0 -> deplacer_case !iz !jz 3;
                    | 0 -> deplacer_case !iz !jz 2;

                end;

            end;

        end;

        remp_graph taille;
        dep_v a b;

    end;;

let rec ratt_end a b =
    search a b;
    match a with
    | _ when a == taille-1 -> begin

        match !iZf with
        | 0 -> begin

            dep_z !iz !jz 2 1 0; search a b;
            dep_z !iz !jz 0 3 0; search a b;
            dep_z !iz !jz 0 1 0; search a b;
            deplacer_case !iz !jz 2; search a b;
            dep_z !iz !jz 2 3 0; search a b;
            dep_z !iz !jz 0 1 0; search a b;
            deplacer_case !iz !jz 0;

        end;
        | _ -> begin

            dep_z !iz !jz 3 2 0;
            ratt_end a b;

        end;

    end;
    | _ -> begin

        match !jZf with
        | 0 -> begin

            dep_z !iz !jz 3 0 0; search a b;
            dep_z !iz !jz 1 2 0; search a b;
            dep_z !iz !jz 1 0 0; search a b;
            deplacer_case !iz !jz 3; search a b;
            dep_z !iz !jz 3 2 0; search a b;
            dep_z !iz !jz 1 0 0; search a b;
            deplacer_case !iz !jz 1;

        end;
        | _ -> begin

            dep_z !iz !jz 2 3 0;
            ratt_end a b;

        end;

    end;;

let check = ref 1;;

let check_fin t =
    for i=0 to t-1 do
        for j=0 to t-1 do
            if taquin_init.(i).(j) <> taquin.(i).(j) then check := 0;
        done;
    done;;

let res t =
    for i=0 to taille-3 do

        for j=0 to taille-3 do
            dep_v i j;
        done;

        dep_v i (taille-2);
        search 0 0;
        deplacer_case !iz !jz 1; remp_graph taille;

        if taquin_init.(i).(taille-1) == taquin.(i).(taille-2) then ratt_end i (taille-1);

        dep_v i (taille-1);

        search i (taille-1);
        if !jZf == 0 then begin
            dep_z !iz !jz 2 3 0;
            search 0 0;
        end;

        dep_z !iz !jz 3 0 0; search 0 0;
        deplacer_case !iz !jz 1;
        remp_graph taille;


    done;
    for j=0 to taille-3 do

        dep_v (taille-2) j;
        search (taille-2) j;
        deplacer_case !iz !jz 0; remp_graph taille;

        search (taille-2) j;

        if !jZf == 1 then deplacer_case !iz !jz 0;

        if taquin_init.(taille-1).(j) == taquin.(taille-2).(j) then ratt_end (taille-1) j;

        dep_v (taille-1) j;

        search (taille-1) j;
        if !iZf == 0 then begin
            dep_z !iz !jz 3 2 0;
            search 0 0;
        end;

        dep_z !iz !jz 2 1 0; search 0 0;
        deplacer_case !iz !jz 0;
        remp_graph taille;

    done;

    dep_v (taille-2) (taille-2);
    remp_graph taille; search 0 0;
    if !jz <> taille-1 then deplacer_case !iz !jz 0;
    if !iz <> taille-1 then deplacer_case !iz !jz 1;

    remp_graph taille;
    print_string "fin du taquin";;


remp_graph taille;;
echange_case taille (50*taille);;
res taille;;


(*
Commandes utiles
cd Users/Jules/Documents/GitHub/Résolution\ Taquin\ Ocaml/ResolveTaquin/base/
#load "graphics.cma";;
#use "taquin.ml";;
*)

*)
remp_graph taille;;
echange_case taille (50*taille);;
