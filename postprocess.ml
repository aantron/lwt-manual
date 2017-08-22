let fmt = Printf.sprintf

let item_name_regexp =
  let open Re in
  seq
    [char ' ';
     group (rep (compl [set " :'"]));
     alt [set " :"; eos]]
  |> compile

let sanity_check soup =
  let open Soup in
  soup $$ "p" |> iter (fun p ->
    p $? "p" |> function
      | None -> ()
      | Some p -> raise Exit)

(* Borrowed from the unreleased postprocessor. *)
let semantic_ocamldoc soup =
  let open Soup in

  (* Remove unnecessary links from <head>. Links will be added in a later pass
     that builds up manual structure. The CSS link will be inserted during
     theming. *)
  let () = soup $$ "head link" |> iter delete in

  (* Remove the navbar. It will be replaced by something else when structure is
     built. *)
  let () = soup $ "div.navbar" |> delete in

  (* Get rid of links in page titles. *)
  let () = soup $$ "h1 a" |> iter unwrap in

  (* Get rid of pointless self-links. *)
  (* TODO The filename will have to be taken on the command line. *)
  let () =
    let self = "Lwt.html" in
    soup $$ "a" |> iter (fun a ->
      match attribute "href" a with
      | Some href when href = self -> unwrap a
      | _ -> ())
  in

  (* Remove horizontal rule. It is not semantic. *)
  let () = soup $ "hr" |> delete in

  (* Remove br elements. They suck. *)
  let () = soup $$ "br" |> iter delete in

  (* Group everything on the page besides the title into a div.interface
     element. This is both for styling and for inlining fragments. *)
  let interface_identifier =
    (* TODO This should be based on the filename found earlier. *)
    let file_title = "Lwt" in
    let page_kind, identifier =
      if Filename.check_suffix file_title "-c" then
        "class", (String.sub file_title 0 (String.length file_title - 2))
      else
        "module", file_title
    in
    let interface_div =
      create_element
        ~attributes:["data-ml-identifier", identifier]
        ~classes:["interface"; page_kind]
        "div"
    in
    soup $ "h1" |> next_siblings |> iter (append_child interface_div);
    append_child (soup $ "body") interface_div;
    identifier
  in

  sanity_check soup;

  (* Put the module tagline into a paragraph. This is necessary to prevent the
     text being coalesced with another paragraph in a subsequent pass. *)
  let () =
    match soup $? ".info.top" with
    | None -> ()
    | Some element_ ->
      let tagline = create_element "p" in
      with_stop begin fun stop ->
        element_ |> children |> iter begin fun child ->
          match element child with
          | Some element when name element = "p" -> stop.throw ()
          | _ -> append_child tagline child
        end
      end;
      prepend_child element_ tagline;
      unwrap element_
  in

  (* Group module/class header and description into a div.top. Nodes that go
     there are everything after the first pre until the next h2 or
      pre:not(.codepre). *)
  let group_info container first_element =
    let siblings =
      match first_element with
      | `After element -> element |> next_siblings |> to_list
      | `Including element ->
        let siblings = element |> next_siblings |> to_list in
        append_child container element;
        siblings
    in
    with_stop (fun stop ->
      siblings |> List.iter (fun node ->
        match element node with
        | None -> append_child container node
        | Some element ->
          match name element with
          | "h2" | "h3" | "h4" | "h5" | "h6" -> stop.throw ()
          | "pre" when not @@
              (List.mem "codepre" (classes element)
              || List.mem "verbatim" (classes element)) ->
            stop.throw ()
          | _ -> append_child container node))
  in

  let () =
    let top_div = create_element ~class_:"top" "div" in
    let header = soup $ ".interface > pre" in
    group_info top_div (`Including header);
    prepend_child (soup $ ".interface") top_div
  in

  (* Group content after section headers. *)
  let () =
    let section_elements =
      soup $$ "*" |> filter (fun element ->
        match name element with
        | "h2" | "h3" | "h4" | "h5" | "h6" -> true
        | _ -> false)
      |> to_list
    in
    section_elements |> List.iter (fun section ->
      let info_div = create_element ~class_:"info" "div" in
      group_info info_div (`After section);
      insert_after section info_div)
  in

  (* Group each item into div.item. *)
  let () =
    (* let interface_div = soup $ ".interface" in *)
    let cursor = create_element ~class_:"cursor" "div" in
    soup $$ ".top ~ pre:not(.nodepre):not(.verbatim)" |> to_list |> List.iter
        (fun item_header ->
      insert_before item_header cursor;
      let item_div = create_element ~class_:"item" "div" in
      group_info item_div (`Including item_header);
      replace cursor item_div);
    assert (soup $? ".cursor" = None)
  in

  (* Separate .top into the pre and the .info. *)
  let () =
    let top_div = soup $ ".top" in
    let info_div = create_element ~class_:"info" "div" in
    top_div $ "pre" |> next_siblings |> iter (append_child info_div);
    append_child top_div info_div
  in

  (* Group everything outside .top into .items. *)
  let () =
    let items_div = create_element ~class_:"items" "div" in
    soup $ ".top" |> next_siblings |> iter (append_child items_div);
    append_child (soup $ ".interface") items_div
  in

  (* Put all inline content in .info into paragraphs. *)
  let () =
    soup $$ ".info" |> iter (fun info ->
      let make_paragraph = function
        | [] -> ()
        | inline ->
          let paragraph = create_element "p" in
          inline |> List.rev |> List.iter (append_child paragraph);
          append_child info paragraph
      in

      let rec group inline_acc = function
        | [] -> make_paragraph inline_acc
        | node::rest ->
          match element node with
          | None -> group (node::inline_acc) rest
          | Some element ->
            match name element with
            | "p" | "pre" | "ul" ->
              make_paragraph inline_acc;
              append_child info node;
              group [] rest
            | _ -> group (node::inline_acc) rest
      in

      group [] (info |> children |> to_list))
  in

  (* Get rid of empty paragraphs. *)
  let () =
    soup $$ ".info > p" |> iter (fun p ->
      if trimmed_texts p = [] then delete p)
  in

  (* Get rid of empty .info elements. *)
  let () =
    soup $$ ".info" |> iter (fun element ->
      if element |> children |> elements |> count = 0 then delete element)
  in

  (* Remove .codepre classes. They are now redundant: all pre.codepre are now
     selectable by .info > pre. code.code is also redundant. *)
  let () =
    soup $$ "pre.codepre" |> iter (remove_class "codepre");
    soup $$ "code.code" |> iter (remove_class "code")
  in

  (* Annotate all items with their kind, identifier, and fully-qualified (long)
     identifier. *)
  let () =
    soup $$ ".item" |> iter (fun item ->
      let kind = item $ ".keyword" |> R.leaf_text in
      set_attribute "data-ml-kind" kind item;
      let name = item $ "pre" |> texts |> String.concat "" |> String.trim in
      let name =
        match Re.all_gen item_name_regexp name () with
        | Some group -> Re.Group.get group 1
        | None ->
          failwith (Printf.sprintf "Could not get name of item '%s'" name)
      in
      set_attribute "data-ml-identifier" name item;
      let fq_name = Printf.sprintf "%s.%s" interface_identifier name in
      set_attribute "data-ml-fq-identifier" fq_name item)
  in

  ()

type table_of_contents =
  section list

and section =
  | Section of {name : string; anchor : string; subsections : table_of_contents}

let top =
  Section {name = "[Top]"; anchor = ""; subsections = []}

let table_of_contents soup =
  let open Soup in
  soup $? "h2" |> function
    | None -> []
    | Some first_section ->
      let h2s =
        first_section
        |> R.previous_sibling
        $$ "~ h2"
      in
      h2s
      |> to_list
      |> List.map begin fun h2 ->
        let h3s =
          with_stop begin fun k ->
            h2 |> next_siblings |> elements |> fold begin fun h3s e ->
              match name e with
              | "h3" -> e::h3s
              | "h2" -> k.throw h3s
              | _ -> h3s
            end []
          end
          |> List.rev
        in
        let subsections =
          h3s |> List.map begin fun h3 ->
            Section
              {name = R.leaf_text h3;
               anchor = R.attribute "id" h3;
               subsections = []}
          end
        in
        Section
          {name = R.leaf_text h2;
           anchor = R.attribute "id" h2;
           subsections}
      end

let markup_for_table_of_contents toc =
  let rec traverse level = function
    | [] -> []
    | (Section section)::rest ->
      let this_section =
        Soup.create_element "a"
          ~class_:(fmt "level-%i" level)
          ~attributes:["href", "#" ^ section.anchor]
          ~inner_text:section.name
      in
      let subsections = traverse (level + 1) section.subsections in
      this_section::(subsections @ traverse level rest)
  in
  traverse 2 toc

let () =
  let open Soup in

  (* Read ocamldoc output from STDIN. *)
  let soup = read_channel stdin |> parse in

  semantic_ocamldoc soup;

  (* Replace the title tag with a bunch of metadata from a file. *)
  read_file "docs/meta.html" |> parse |> replace (soup $ "title");

  (* Get rid of the pointless : sig .. end stuff. *)
  let trim_module_declaration e =
    let text_node = children e |> R.nth 2 in
    let content = texts text_node |> String.concat "" in
    (String.length content) - 2
    |> String.sub content 0
    |> create_text
    |> replace text_node;

    let sig_keyword = e $ "code" in
    sig_keyword |> next_siblings |> iter delete;
    delete sig_keyword
  in
  soup $ ".top > pre" |> trim_module_declaration;

  (* Helper for text replacements. *)
  let text_nodes e =
    e |> descendants |> filter (fun e -> not (is_element e))
  in

  (* debug_consistency_check soup; *)

  (* Tag acronyms. *)
  let () =
    let acronyms = ["I/O"; "PPX"; "STDIN"] in
    let regexp =
      let open Re in
      acronyms
      |> List.map str
      |> alt
      |> compile
    in

    soup $$ "p" |> iter begin fun p ->
      p |> text_nodes |> iter begin fun t ->
        texts t
        |> String.concat ""
        |> Re.split_full regexp
        |> List.map begin function
          | `Text t -> create_text t
          | `Delim g ->
            create_element
              "span" ~class_:"acronym" ~inner_text:(Re.Group.get g 0)
            |> Obj.magic
          end
        |> function
          | [] -> delete t
          | first::rest ->
            replace t first;
            let rec insert_rest last = function
              | [] -> ()
              | current::rest ->
                insert_after last current;
                insert_rest current rest
            in
            insert_rest first rest
      end
    end
  in

  (* Get rid of syntax highlighting in inline code. *)
  let remove_highlighting code =
    texts code
    |> String.concat ""
    |> create_text
    |> fun text_node ->
      clear code;
      append_child code text_node
  in
  soup $$ "p code" |> iter remove_highlighting;
  soup $$ "li code" |> iter remove_highlighting;

  (* Greek character replacement. *)
  let greek_replacement =
    let re =
      Re.(seq [char '\''; group (set "abcdefgh")])
      |> Re.compile
    in

    fun e ->
      e |> descendants |> filter (fun e -> not @@ is_element e) |> iter
          begin fun text_node ->
        text_node
        |> R.leaf_text
        |> Re.replace re ~f:(fun groups ->
          match Re.Group.get groups 1 with
          | "a" -> "α"
          | "b" -> "β"
          | "c" -> "γ"
          | "d" -> "δ"
          | "e" -> "ε"
          | "f" -> "ζ"
          | "g" -> "η"
          | "h" -> "θ"
          | _ -> assert false)
        |> create_text
        |> replace text_node
      end
  in
  soup $$ ".item > pre" |> iter greek_replacement;
  soup $$ "p code" |> iter greek_replacement;
  soup $$ "li code" |> iter greek_replacement;

  (* Arrow replacement. *)
  (* Note: the nice two-character arrows are actually coming from STIXGeneral,
     which is probably only installed by macOS by default. When Chrome fails to
     find a character for the long arrow in Courier, it hits monospace, and
     looks up the arrow in STIXGeneral. *)
  let arrow_replacement =
    let re = Re.str "->" |> Re.compile in

    fun e ->
      e |> descendants |> filter (fun e -> not @@ is_element e) |> iter
          begin fun text_node ->
        text_node
        |> R.leaf_text
        |> Re.replace_string re ~by:"⟶"
        |> create_text
        |> replace text_node
      end
  in
  soup $$ ".item > pre" |> iter arrow_replacement;

  (* Have code links swallow up preceding code. This affects type parameters and
     dereferencing operators. *)
  let () =
    soup
    $$ "a > code"
    |> map R.parent
    |> iter begin fun a ->
      match previous_sibling a with
      | None -> ()
      | Some node ->
        match element node with
        | None -> ()
        | Some element ->
          if name element <> "code" then
            ()
          else
            match leaf_text element with
            | None -> ()
            | Some text ->
              let new_text =
                text ^ (R.leaf_text a)
                |> create_text
              in
              let code = a $ "code" in
              clear code;
              append_child code new_text;
              delete element
    end
  in

  let () =
    let toc = table_of_contents soup in
    let toc =
      toc |> List.filter (fun (Section s) ->
        s.name <> "Deprecated")
    in
    let toc =
      (Section {name = "[Top]"; anchor = ""; subsections = []})::
      (Section {name = "Quick start"; anchor = "3_Quickstart"; subsections = []})::
      (Section {name = "Tutorial"; anchor = "3_Tutorial"; subsections = []})::
      toc
    in

    let toc_div = create_element "div" ~class_:"toc" in

    toc
    |> markup_for_table_of_contents
    |> List.iter (fun e ->
      append_child toc_div (create_element "br");
      append_child toc_div e);

    toc_div $ "br" |> delete;

    create_element "h3" ~inner_text:"Table of contents"
    |> prepend_child toc_div;

    soup $ "h2" |> fun h2 -> insert_before h2 toc_div
  in

  (* Replace the <h1> element with a new header from header.html. *)
  read_file "docs/header.html" |> parse |> replace (soup $ "h1");

(*
  (* Remove the nav bar, horizontal rule, and some line breaks. *)
  soup $ ".navbar" |> delete;
  soup $ "hr" |> delete;
  soup $$ "body > br" |> iter delete;

  (* Remove unnecessary links to self. Replace them with their content. *)
  soup $$ "a:contains(\"..\")" |> iter unwrap;

  (* The Infix module should somehow be inlined. *)
  (*soup $ "a:contains(\"Infix\")" |> unwrap;*)

  (* Generate a table of contents after the module information text. For wide
     (desktop) screens, CSS will move this to the top-left corner in fixed
     position. *)
  let table_of_contents =
    (* List all the sections - get their ids and labels. *)
    let sections =
      soup $$ "h2"
      |> to_list
      |> List.map (fun h2 -> R.id h2, R.leaf_text h2)
    in

    (* Create a div to hold the entire table of contents. This is the element
       that is conditionally positioned. *)
    let div = create_element ~class_:"toc" "div" in

    (* Give the TOC a heading. This is only displayed at narrow widths. *)
    create_element ~inner_text:"Module contents" "p" |> append_child div;

    (* Generate a nested div to hold only the links. This element has a
       multi-column style applied to it on narrow displays. *)
    let links = create_element ~class_:"links" "div" in
    append_child div links;

    (* Iterate over the pairs of section id/section label. Add an anchor to the
       div just created for each section. Include a [Top] link first. *)
    ("", "[Top]")::sections |> List.iter (fun (id, title) ->
      create_element ~attributes:["href", "#" ^ id] ~inner_text:title "a"
      |> append_child links;
      create_element "br" |> append_child links);

    (* Separate the [Top] link from the rest by a line break. *)
    create_element "br" |> insert_after (div $ "a");

    (* Add some blank lines before the GitHub link. *)
    create_element "br" |> append_child div;
    create_element "br" |> append_child div;

    (* Add the GitHub link at the bottom of the table of contents. *)
    create_element
      ~attributes:["href", "https://github.com/aantron/lambda-soup"]
      ~classes:["github"; "hide-narrow"] ~inner_text:"GitHub"
      "a"
    |> append_child div;

    (* Hide the [Top] link if the display gets narrow. Since the table of
       contents becomes statically (normally) positioned at narrow widths, the
       top link will scroll off screen when scrolling away from the top, and
       thus become useless for returning to the top. *)
    div $ "a" |> set_attribute "class" "hide-narrow";

    (* Finally, evaluate to the TOC container div. *)
    div
  in

  (* Place the table of contents at the end of the module description. *)
  append_child (soup $$ ".info" |> R.nth 2) table_of_contents;

  (* Find every multi-line signature member declaration, and add a class to its
     info block. This class allow special styling with CSS, such as a wider top
     margin. *)
  soup $$ "pre"
  |> filter (fun e -> e $? ".type" <> None)
  |> filter (fun e -> e $? "br" <> None)
  |> iter (fun e ->
    match e $? "+ .info" with
    | None -> ()
    | Some e -> e |> add_class "multiline-member");

  (* Find every section that has additional text after the title, and add a
     class to the last element of such text. This is used by CSS to increase
     spacing. *)
  soup $$ "h2" |> iter (fun h2 ->
    let e = h2 $ "~ pre:not(.codepre)" |> R.previous_element in
    if name e = "h2" then ()
    else add_class "end-of-section-text" e);

  (* Clean up links in the head. *)
  soup $$ "head link:not([rel=stylesheet])" |> iter delete;

  (* Replace the title tag with a bunch of metadata from file. *)
  read_file "meta.html" |> parse |> replace (soup $ "title");

  (* Fix up internal cross-references by dropping the module prefix, and
     correcting the destination. *)
(*
  soup $$ "a[href^=Soup.html#]"
  |> iter (fun a ->
    let href = a |> R.attribute "href" in
    String.sub href 9 (String.length href - 9)
    |> fun v -> set_attribute "href" v a;

    let text = texts a |> String.concat "" in
    let starts_with_module =
      try String.sub text 0 5 = "Soup."
      with Invalid_argument _ -> false
    in

    if starts_with_module then
      let value_name = String.sub text 5 (String.length text - 5) in
      clear (R.child a);
      create_text value_name |> append_child (a |> R.child_element));
*)
  (* Insert clickable anchors. *)
  soup $$ "span[id]" |> iter (fun span ->
    set_name "a" span;
    set_attribute "href" ("#" ^ (R.attribute "id" span)) span);

  soup $$ "h2[id]" |> iter (fun h2 ->
    let href = "#" ^ (R.attribute "id" h2) in
    let a =
      create_element
        ~attributes:["href", href] ~inner_text:(R.leaf_text h2) "a";
    in
    clear h2;
    append_child h2 a);

  soup $$ "h2 ~ pre > span.keyword:contains('module')" |> iter (fun span ->
    let pre = R.parent span in
    let name = R.nth 3 (children pre) |> R.leaf_text |> String.trim in
    let anchor = fmt "MODULE%s" name in
    let replacement =
      fmt
        "<a href='#%s' id='%s'><span class='keyword'>module</span> %s</a> : %s"
        anchor anchor name
        ("<code class='code'><span class='keyword'>sig</span></code>" ^
         " .. " ^
         "<code class='code'><span class='keyword'>end</span></code>")
    in
    clear pre;
    parse replacement |> children |> iter (append_child pre));

  soup $$ "li code" |> iter begin fun code ->
    texts code
    |> String.concat ""
    |> create_text
    |> fun text_node ->
      clear code;
      append_child code text_node
  end;
*)
  (* Convert the soup back to HTML and write to STDOUT. The Makefile redirects
     that to the right output file. *)
  soup |> to_string |> write_channel stdout
