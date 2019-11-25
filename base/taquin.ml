open Graphics;;
module R = Random;;
module A = Array;;
module U = Unix;;

let taille = 7;;
let div = 700/taille;;

open_graph " 700x700";;
let taquin = A.make_matrix taille taille 0;;
let taquin_init = A.make_matrix taille taille 0;;
let taquin_save = A.make_matrix taille taille 0;;


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
                    taquin_save.(i).(j) <- 0;
                end
            else
                begin
                    taquin.(i).(j) <- i*taille+j+1;
                    taquin_init.(i).(j) <- i*taille+j+1;
                    taquin_save.(i).(j) <- i*taille+j+1;
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


let icase = ref 0;; (* variable contenant le i de la case qu'on cherche a placer*)
let jcase = ref 0;; (* variable contenant le j de la case qu'on cherche a placer*)
let inextcase = ref 0;;
let jnextcase = ref 0;;
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

  let search_next case=
    for i=0 to taille-1 do
      for j=0 to taille-1 do
        if taquin.(i).(j)==case then
        begin
          inextcase:=i;
          jnextcase:=j;
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
  search case;
    (*faire des tours pour la placer sue le côté*)
    while !icase != !ligneactuelle && !icase != taille-1 && !jcase != !colonneactuelle && !jcase != taille-1 do
      search case;
      print_int (!ivide - !icase);
      getposblack 0;
      for i=0 to (taille -1 - !jvide-1) do
        deplacer_case !ivide !jvide 0;
        getposblack 0;
        remp_graph taille;
      done;
        deplacer_case !ivide !jvide 3;
        getposblack 0;
        remp_graph taille;
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

let rotate2 case =
  search case;
    (*faire des tours pour la placer sue le côté*)
    while !icase != !ligneactuelle && !icase != taille-1 && !jcase != !colonneactuelle && !jcase != taille-1 do
      print_int (!ivide - !icase);
      getposblack 0;
      for i=0 to taille -1 - !jvide-1 do
        deplacer_case !ivide !jvide 0;
        getposblack 0;
        remp_graph taille;
      done;
      for i=0 to 1 do
        deplacer_case !ivide !jvide 3;
        getposblack 0;
        remp_graph taille;
      done;
      for i=0 to taille -2 do
        deplacer_case !ivide !jvide 2;
        getposblack 0;
        remp_graph taille;
      done;
      for i=0 to 1 do
        deplacer_case !ivide !jvide 1;
        getposblack 0;
        remp_graph taille;
      done;
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
    if !ivide - !icase <= 1 then begin
    rotate case;
    end;
    if (!ivide - !icase > 1) && (!icase != !ligneactuelle || !icase != taille-1 || !jcase != !colonneactuelle || !jcase != taille-1) then begin
      rotate2 case;
    end;
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

  (*déplace un cercle de case d'une case*)
  let tourne case =
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
      for i=0 to (taille - 2 - !ligneactuelle) do
        deplacer_case !ivide !jvide 1;
        getposblack 0;
        remp_graph taille;
      done;
      search case;;

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

        get_under case;
        deplacer_case !ivide !jvide 3;
        getposblack 0;
        remp_graph taille;
    end;;

    let placeFin_debug2 case =
      get_under (case+1);
      deplacer_case !ivide !jvide 3;
      getposblack 0;
      remp_graph taille;
      deplacer_case !ivide !jvide 0;
      getposblack 0;
      remp_graph taille;
      deplacer_case !ivide !jvide 1;
      getposblack 0;
      remp_graph taille;
      deplacer_case !ivide !jvide 2;
      getposblack 0;
      remp_graph taille;
      placeFin_debug case;
      placeFin_debug (case+1);;






(*fonction qui place la case en question *)
let place case etape =
  get_on_side case;
  search_resolve case;
  (*juste un rotate sur tout le bord pour la placer*)
  while !icase != !icaseres || !jcase != !jcaseres do
    tourne case;
  done;;

(*fonction qui place les 2 dernières cases de la ligne*)
let place_prepa_fin case etape=
    search case;
    search_resolve case;
    get_on_side case;
    (*Même chose sauf qu'on vérifie si la case est au bon endroit sinon ça boucle pour rien*)
    while (!icase != !ligneactuelle || !jcase != taille - 1) && (!icase != !ligneactuelle -1 || !jcase != taille - 1) && (!icase != !ligneactuelle -1 || !jcase != taille - 2) do
        tourne case;
    done;
    (*On se place en dessous et on place ensuite la case dans le coin en haut a droite*)
    get_under case;;

(*prépare la première case de la colonne à mettre*)
let prepare_first case=
  search (case+taille);
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

    search (case+taille);
  done;;

let prepare_col case=
  prepare_first case;
  getposblack 0;

  search case;
  (*pour éviter que la case  devienne inaccessible*)
  while !icase == (!icaseres+1) && !jcase == !jcaseres do
    if !ivide==taille-1 then
    begin
      print_string "dep haut\n";
      deplacer_case !ivide !jvide 3;
      getposblack 0;

    end;
    remp_graph taille;
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
 getposblack 0;
 etape :=0;
 for i=0 to (taille-2 - !jvide) do
   deplacer_case !ivide !jvide 0;
   getposblack 0;
   remp_graph taille;
 done;
 for i=0 to (taille-2 - !ivide) do
   deplacer_case !ivide !jvide 1;
   getposblack 0;
   remp_graph taille;
 done;
 for i=0 to (taille-2 - !jvide) do
   deplacer_case !ivide !jvide !etape;
   getposblack 0;
   remp_graph taille;
 done;
 while !icase != !icaseres || !jcase != !jcaseres do

  if !etape==4 then
    etape:=0;
  if !etape==0 then
  begin
    print_string "droite\n";
      for i=0 to (taille-2 - !jvide) do
        deplacer_case !ivide !jvide !etape;
        getposblack 0;
        remp_graph taille;
      done;
      etape := !etape+1;
  end
  else if !etape==1 then
  begin
    print_string "bas\n";
    for i=0 to (taille - 2 - !ivide) do
      deplacer_case !ivide !jvide 1;
      getposblack 0;
      remp_graph taille;
    done;
    etape := !etape+1;
  end
  else begin
    print_string "autre\n";
    deplacer_case !ivide !jvide !etape;
    getposblack 0;
    remp_graph taille;
    etape := !etape+1;
  end;

  search case;

 done;
 for i=0 to (taille - 2 - !ivide) do
   deplacer_case !ivide !jvide 1;
   getposblack 0;
   remp_graph taille;
 done;;

 let resolve taille =
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
   (*On reteste une fois si 4 est pas déjà placer avec le déplacement précédent*)
   placeFin_debug !caseactuelle;

   search_next (!caseactuelle+1);
    if(!jnextcase == !jcaseres && !jcase== (!jcaseres+1) && !icase == !inextcase) then begin
     placeFin_debug2 !caseactuelle;
    end;

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
read_line();;
resolve taille;;


(*#use "taquin.ml";;*)
