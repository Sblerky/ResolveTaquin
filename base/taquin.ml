open Graphics;;
module R = Random;;
module A = Array;;
module U = Unix;;

let taille = 7;;
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
    (*ignore (Graphics.read_key ());;*)

let remp_graph_pause n =
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
    done;
    read_line();;


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


let icase = ref 0;; (* variable contenant le i de la case qu'on cherche a placer*)
let jcase = ref 0;; (* variable contenant le j de la case qu'on cherche a placer*)
let icaseres = ref 0;; (* variable contenant le i final de la case qu'on cherche a placer*)
let jcaseres = ref 0;; (* variable contenant le j final de la case qu'on cherche a placer*)
let ivide = ref 0;; (* variable contenant le i de la case noire*)
let jvide = ref 0;; (* variable contenant le j de la case noire*)
let ligneactuelle = ref 0;; (* variable contenant l'indice de la ligne en cours de traitement*)
let colonneactuelle = ref 0;; (* variable contenant l'indice de la colonne en cours de traitement*)
let caseactuelle = ref 1;; (* variable contenant l'indice de la case en cours de traitement*)
let etape = ref 2;;



(*Fonction qui remplit icase et jcase*)
let search case=
  for i=0 to taille-1 do
    for j=0 to taille-1 do
      if taquin.(i).(j)==case then
      begin
        icase:=i;
        jcase:=j;
      end
    done;
  done;;

let search_resolve case=
  for i=0 to taille-1 do
    for j=0 to taille-1 do
      if taquin_init.(i).(j)==case then
      begin
        icaseres:=i;
        jcaseres:=j;
      end
    done;
  done;;

(*Fonction qui remplit ivide et jvide, prend un paramètre sinon s'éxécute toute seule*)
let getposblack case=
  for i=0 to taille-1 do
    for j=0 to taille-1 do
      if taquin.(i).(j)==0 then
      begin
        ivide:=i;
        jvide:=j;
      end
    done;
  done;;



(*Fonction qui place la case noire sous la case a traiter, a appeler après avoir vérifier que la case
n'est pas sur le contour*)
let get_under case=
  search case;
  getposblack 0;
  if !icase == !ivide then
  begin
    (*déplacement à gauche puis en bas puis à droite, fonctionne*)
    for i=0 to !jvide-1 do
      deplacer_case !ivide !jvide 2;
      getposblack 0;
      remp_graph taille;
    done;
    deplacer_case !ivide !jvide 1;
    getposblack 0;
    search case;
    remp_graph taille;
    for i=0 to !jcase-1 do
      deplacer_case !ivide !jvide 0;
      getposblack 0;
      remp_graph taille;
    done;
  end

  else if !icase < !ivide then
  begin
    (*tester l'écart puis déplacer en haut si >1, sinon chercher dans quel sens partir*)
    if !ivide - !icase > 1 then
    begin
      for i=0 to !ivide - !icase -2 do
        deplacer_case !ivide !jvide 3;
        getposblack 0;
        remp_graph taille;
      done;
      (*si noire à gauche de case*)
      if !jcase - !jvide > 0 then
      begin
        for i=0 to !jcase - !jvide -1 do
          deplacer_case !ivide !jvide 0;
          getposblack 0;
          remp_graph taille;
        done;
      end
      (*si noire à droite de case*)
      else if !jcase - !jvide < 0 then
      begin
        for i=0 to !jvide - !jcase -1 do
          deplacer_case !ivide !jvide 2;
          getposblack 0;
          remp_graph taille;
        done;
      end
    end
    (*si directement ligne en dessous*)
    else if !jcase - !jvide > 0 then
    begin
      for i=0 to !jcase - !jvide -1 do
        deplacer_case !ivide !jvide 0;
        getposblack 0;
        remp_graph taille;
      done;
    end
    (*si noire à droite de case*)
    else if !jcase - !jvide < 0 then
    begin
      for i=0 to !jvide - !jcase -1 do
        deplacer_case !ivide !jvide 2;
        getposblack 0;
        remp_graph taille;
      done;
    end

  end

  else if !icase > !ivide then
  begin
    (*tester l'écart, puis déplacer en bas +1 puis horizontal*)
    for i=0 to !icase - !ivide do
      deplacer_case !ivide !jvide 1;
      search case;
      getposblack 0;
      remp_graph taille;
    done;
    (*si noire à gauche de case*)
    if !jcase - !jvide > 0 then
    begin
      for i=0 to !jcase - !jvide -1 do
        deplacer_case !ivide !jvide 0;
        getposblack 0;
        remp_graph taille;
      done;
    end
    (*si noire à droite de case*)
    else if !jcase - !jvide < 0 then
    begin
      for i=0 to !jvide - !jcase -1 do
        deplacer_case !ivide !jvide 2;
        getposblack 0;
        remp_graph taille;
      done;
    end
  end;;



(*rotate sur la droite dans le sens anti-horaire en prenant en compte ligne actuelle + case vide sur le contour*)
let rotate case =
  (*faire des tours pour la placer sue le côté*)
  while !icase != !ligneactuelle && !icase != taille-1 && !jcase != !colonneactuelle && !jcase != taille-1 do
    getposblack 0;
    for i=0 to (taille -1 - !jvide-1) do
      deplacer_case !ivide !jvide 0;
      getposblack 0;
      remp_graph taille;
    done;
    deplacer_case !ivide !jvide 3;
    getposblack 0;
    for i=0 to (taille -2) do
      deplacer_case !ivide !jvide 2;
      getposblack 0;
      remp_graph taille;
    done;
    deplacer_case !ivide !jvide 1;
    getposblack 0;
    remp_graph taille;
    search case;
  done;;

(*fonction a appeler dans les cas particuliers de get_on_side*)
let get_on_side_debug case=
  search case;
  getposblack 0;

  (*si la case est en bas à gauche*)
  if !icase == taille-1 then
  begin
    (*on met la case noire tout en bas*)
    for i=0 to (taille -1 - !ivide-1) do
      deplacer_case !ivide !jvide 1;
      search case;
      getposblack 0;
      remp_graph taille;
    done;
    search case;

    (*genre de rotate pour la mettre dans le carré (duplication de code super)*)

    while !jcase <= (!colonneactuelle+1) do
      getposblack 0;
      for i=0 to (taille -1 - !jvide-1) do
        deplacer_case !ivide !jvide 0;
        getposblack 0;
        remp_graph taille;
      done;
      deplacer_case !ivide !jvide 3;
      getposblack 0;
      for i=0 to (taille -2) do
        deplacer_case !ivide !jvide 2;
        getposblack 0;
        remp_graph taille;
      done;
      deplacer_case !ivide !jvide 1;
      getposblack 0;
      remp_graph taille;
      search case;
    done;
    (*quand c'est fini on remet la case noire à sa place*)
    for i=0 to (taille -1 - !jvide-1) do
      deplacer_case !ivide !jvide 0;
      getposblack 0;
      remp_graph taille;
    done;
    search case;
    rotate case;
  end
  (*sinon traitement normal*)
  else begin
    get_under case;
    rotate case;
    for i=0 to (taille -1 - !jvide-1) do
      deplacer_case !ivide !jvide 0;
      getposblack 0;
      remp_graph taille;
    done;
  end;;

(*Fonction qui place la case à traiter sur le contour défini par les lignes/col déjà traitées
 si elle n'y est pas déjà*)
let get_on_side case =
  search case;
  getposblack 0;
  if (!icase == !ligneactuelle || !icase == taille-1 || !jcase == !colonneactuelle || !jcase == taille-1)
  && (!icase != taille-1 || !jcase >= !colonneactuelle)
  then
  begin
    (*placer la case vide sur le contour en bas
     puis tester si case à traiter y est toujours et si oui aller à droite*)
    for i=0 to (taille -1 - !ivide-1) do
      deplacer_case !ivide !jvide 1;
      search case;
      getposblack 0;
      remp_graph taille;
    done;

    search case;

    if (!icase == !ligneactuelle || !icase == taille-1 || !jcase == !colonneactuelle || !jcase == taille-1)
    && (!icase != taille-1 || !jcase > !colonneactuelle)
    then
      begin
        for i=0 to (taille -1 - !jvide-1) do
          deplacer_case !ivide !jvide 0;
          getposblack 0;
          remp_graph taille;
        done;
    end
    else (*autre traitement*)
    begin
      get_on_side_debug case;
    end
  end

  else
  begin
    get_on_side_debug case;
  end;;

(*Place la case voulu en haut à droite, on l'utilise après place_prepa_fin*)
let place_coin_haut case =
if(!icase == !ligneactuelle && !jcase == taille - 1) then
begin
    (*gauche haut haut droite bas*)
    deplacer_case !ivide !jvide 2;
    getposblack 0;
    remp_graph taille;
    deplacer_case !ivide !jvide 3;
    getposblack 0;
    remp_graph taille;
    deplacer_case !ivide !jvide 3;
    getposblack 0;
    remp_graph taille;
    deplacer_case !ivide !jvide 0;
    getposblack 0;
    remp_graph taille;
    deplacer_case !ivide !jvide 1;
    getposblack 0;
    remp_graph taille;
end;;

(*Debug pour remettre les cases de fin à l'endroit voulu sinon la ligne est décalé*)
let placeFin_debug case =
    search case;
    search_resolve case;
    if(!icase == !icaseres && !jcase == !jcaseres) then begin
        get_under case;
        (*haut , gauche ou droite, bas*)
        deplacer_case !ivide !jvide 3;
        getposblack 0;
        remp_graph taille;
        if(!jvide != taille-1) then begin (*Si c'est le 6 qui est mal placé*)
            deplacer_case !ivide !jvide 0;
            getposblack 0;
            remp_graph taille;
        end
        else begin (*Sinon si c'est le 7*)
            deplacer_case !ivide !jvide 2;
            getposblack 0;
            remp_graph taille;
        end;
        deplacer_case !ivide !jvide 1;
        getposblack 0;
        remp_graph taille;
    end;;

(*fonction qui place la case en question *)
let place case etape =
  get_on_side case;
  search_resolve case;
  (*juste un rotate sur tout le bord pour la placer*)
  while !icase != !icaseres || !jcase != !jcaseres do
    getposblack 0;
    for i=0 to (taille -1 - !jvide-1) do
      deplacer_case !ivide !jvide 0;
      getposblack 0;
      remp_graph taille;
    done;
    for i=0 to (!ivide-1 - !ligneactuelle) do
      deplacer_case !ivide !jvide 3;
      getposblack 0;
      remp_graph taille;
    done;
    for i=0 to (taille -2 - !colonneactuelle) do
      deplacer_case !ivide !jvide 2;
      getposblack 0;
      remp_graph taille;
    done;
    for i=0 to (taille - etape) do   (*faut incrémenter etape a chaque fois qu'on change de ligne (avant cétait toujours 2)*)
      deplacer_case !ivide !jvide 1;
      getposblack 0;
      remp_graph taille;
    done;
    search case;
  done;;

(*fonction qui place les 2 dernières cases de la ligne*)
let place_prepa_fin case etape=
    search case;
    search_resolve case;
    get_on_side case;
    (*Même chose sauf qu'on vérifie si la case est au bon endroit sinon ça boucle pour rien*)
    while (!icase != !ligneactuelle || !jcase != taille - 1) && (!icase != !ligneactuelle -1 || !jcase != taille - 1) && (!icase != !ligneactuelle -1 || !jcase != taille - 2) do
        getposblack 0;
        for i=0 to (taille -1 - !jvide-1) do
            deplacer_case !ivide !jvide 0;
            getposblack 0;
            remp_graph taille;
        done;
        for i=0 to (!ivide-1 - !ligneactuelle) do
            deplacer_case !ivide !jvide 3;
            getposblack 0;
            remp_graph taille;
        done;
        if(!icase != !ligneactuelle || !jcase != taille - 1) then
        for i=0 to (taille -2 - !colonneactuelle) do
            deplacer_case !ivide !jvide 2;
            getposblack 0;
            remp_graph taille;
        done;
        for i=0 to (taille - etape) do
            deplacer_case !ivide !jvide 1;
            getposblack 0;
            remp_graph taille;
        done;
        search case;
    done;
    (*On se place en dessous et on place ensuite la case dans le coin en haut a droite*)
    get_under case;;

(*prépare la première case de la colonne à mettre*)
let prepare_first case=
  search (case+7);
  search_resolve case;
  getposblack 0;
  (*tant que la case est pas la ou elle doit être on fait le tour*)
  while !icase != !icaseres || !jcase != !jcaseres do
    print_string "dep 1\n";
    for i=0 to (taille-2 - !colonneactuelle) do
      deplacer_case !ivide !jvide 2;
      getposblack 0;
      remp_graph taille;
    done;
    print_string "dep 2\n";
    for i=0 to (taille - 2 - !ivide) do
      deplacer_case !ivide !jvide 1;
      getposblack 0;
      remp_graph taille;
    done;
    print_string "dep 3\n";
    for i=0 to (taille-2 - !jvide) do
      deplacer_case !ivide !jvide 0;
      getposblack 0;
      remp_graph taille;
    done;
    print_string "dep 4\n";
    deplacer_case !ivide !jvide 3;
    getposblack 0;
    remp_graph taille;

    search (case+7);
  done;;

let prepare_col case=
  prepare_first case;
  getposblack 0;
  search case;
  (*pour éviter que la case  devienne inaccessible*)
  while !icase == (!icaseres+1) && !jcase == !jcaseres do
    print_string "dep 5\n";
    for i=0 to (taille-2 - !colonneactuelle) do
      deplacer_case !ivide !jvide 2;
      getposblack 0;
      remp_graph taille;
    done;

    print_string "dep 6\n";
    deplacer_case !ivide !jvide 1;
    getposblack 0;
    remp_graph taille;

    print_string "dep 7\n";
    deplacer_case !ivide !jvide 0;
    getposblack 0;
    remp_graph taille;

    print_string "dep 8\n";
    deplacer_case !ivide !jvide 3;
    getposblack 0;
    remp_graph taille;

    print_string "dep 9\n";
    deplacer_case !ivide !jvide 0;
    getposblack 0;
    remp_graph taille;

    print_string "dep 10\n";
    deplacer_case !ivide !jvide 1;
    getposblack 0;
    remp_graph taille;

    for i=0 to (taille-2 - !jvide) do
      deplacer_case !ivide !jvide 0;
      getposblack 0;
      remp_graph taille;
    done;

    prepare_first case;
    search (case+7);
  done;

  print_string "dep 11\n";
  for i=0 to (taille - 2 - !ivide) do
    deplacer_case !ivide !jvide 1;
    getposblack 0;
    remp_graph taille;
  done;

  print_string "dep 12\n";
  for i=0 to (taille-2 - !jvide) do
    deplacer_case !ivide !jvide 0;
    getposblack 0;
    remp_graph taille;
  done;

  search case;

  while !icase != !icaseres || !jcase != (!jcaseres+1) do
    print_string "dep 13\n";
    for i=0 to (taille-2 - (!colonneactuelle+1)) do
      deplacer_case !ivide !jvide 2;
      getposblack 0;
      remp_graph taille;
    done;
    print_string "dep 14\n";
    for i=0 to (taille - 2 - !ivide) do
      deplacer_case !ivide !jvide 1;
      getposblack 0;
      remp_graph taille;
    done;
    print_string "dep 15\n";
    for i=0 to (taille-2 - !jvide) do
      deplacer_case !ivide !jvide 0;
      getposblack 0;
      remp_graph taille;
    done;
    print_string "dep 16\n";
    deplacer_case !ivide !jvide 3;
    getposblack 0;
    remp_graph taille;

    search case;
  done;
  print_string "dep 17\n";
  for i=0 to (taille - 2 - !ivide) do
    deplacer_case !ivide !jvide 1;
    getposblack 0;
    remp_graph taille;
  done;
  print_string "dep 18\n";
  for i=0 to (taille-2 - !colonneactuelle) do
    deplacer_case !ivide !jvide 2;
    getposblack 0;
    remp_graph taille;
  done;
  print_string "dep 19\n";
  deplacer_case !ivide !jvide 3;
  getposblack 0;
  remp_graph taille;
  print_string "dep 20\n";
  deplacer_case !ivide !jvide 0;
  getposblack 0;
  remp_graph taille;
  for i=0 to (taille - 2 - !ivide) do
    deplacer_case !ivide !jvide 1;
    getposblack 0;
    remp_graph taille;
  done;
  for i=0 to (taille-2 - !jvide) do
    deplacer_case !ivide !jvide 0;
    getposblack 0;
    remp_graph taille;
  done;;

let last_piece case=
 search_resolve case;
 search case;
 etape :=0;
 while !icase != !icaseres || !jcase != !jcaseres do
   deplacer_case !ivide !jvide !etape;
   getposblack 0;
   remp_graph taille;
   etape := !etape+1;

   if !etape == 4 then
   begin
     etape:=0;
   end
 done;;






let resolve_taquin x=
  for i=0 to taille-3 do
   (*placement premières cases*)
    for j=1 to taille-2 do
      place !caseactuelle !etape;
      caseactuelle := !caseactuelle+1;
      colonneactuelle := !colonneactuelle+1;
    done;
    colonneactuelle := 0;
    ligneactuelle := !ligneactuelle+1;
    etape := !etape+1;
    (*placement dernières cases*)
    placeFin_debug !caseactuelle;
    placeFin_debug (!caseactuelle+1);
    place_prepa_fin !caseactuelle !etape;
    place_coin_haut !caseactuelle;
    place_prepa_fin (!caseactuelle+1) !etape;
    place_coin_haut (!caseactuelle+1);
    caseactuelle := !caseactuelle+2;
  done;

  for i = 0 to taille -3 do
    prepare_col !caseactuelle;
    colonneactuelle := (!colonneactuelle +1);
    caseactuelle := (!caseactuelle +1);
  done;
  last_piece !caseactuelle;;






remp_graph taille;;
echange_case taille (50*taille);;
resolve_taquin 0;;

(*
prepare_col 36;;
colonneactuelle := 1;;

prepare_col 37;;
colonneactuelle := 2;;

prepare_col 38;;
colonneactuelle := 3;;

prepare_col 39;;
colonneactuelle := 4;;

prepare_col 40;;
colonneactuelle := 5;;
*)

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
