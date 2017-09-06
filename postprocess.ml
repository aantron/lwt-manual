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

  (* Read Lwt.Infix and inline it. *)
  let () =
    let infix_soup = read_file "../lwt/doc/api/html/Lwt.Infix.html" |> parse in

    let module_brief = soup $ "body > pre:contains('Infix') ~ div" in
    let destination = soup $ "body > pre:contains('Infix') ~ br" in

    let module_info = infix_soup $ "pre + div" in
    let module_items = infix_soup $$ "hr ~ *" |> to_list in

    (* Copy over the module full description. Lwt.html has only the brief
       description. *)
    replace module_brief module_info;

    (* Copy over the module contents. *)
    module_items |> List.iter begin fun item ->
      begin match name item with
      | "pre" ->
        let value_name_node = item $ "span span" |> R.next_sibling in
        let value_name = R.leaf_text value_name_node |> String.trim in
        " Infix." ^ value_name
        |> create_text
        |> replace value_name_node;
        (* Adjust the id. *)
        let span = item $ "span" in
        span
        |> R.attribute "id"
        |> fun s -> "Infix." ^ s
        |> fun s -> set_attribute "id" s span
      | _ ->
        ()
      end;
      insert_before destination item
    end
  in

  semantic_ocamldoc soup;

  (* Replace the title tag with a bunch of metadata from a file. *)
  read_file "docs/meta.html" |> parse |> replace (soup $ "title");

  let () =
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
    soup $ ".item > pre:contains('Infix') a"
    |> next_siblings
    |> iter delete
  in

  let () =
    (* Adjust the inlined Infix module markup. *)
    soup $ ".item > pre:contains('Infix') a" |> unwrap;
    soup $ ".item > pre:contains('Infix') span"
    |> set_attribute "id" "MODULEInfix"
  in

  (* Helper for text replacements. *)
  let text_nodes e =
    e |> descendants |> filter (fun e -> not (is_element e))
  in

  (* debug_consistency_check soup; *)

  (* Tag acronyms. *)
  let () =
    let acronyms = ["I/O"; "PPX"; "STDIN"; "STDOUT"; "CPU"; "API"] in
    let regexp =
      let open Re in
      acronyms
      |> List.map str
      |> alt
      |> compile
    in

    let in_elements name =
      soup $$ name |> iter begin fun p ->
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

    in_elements "p";
    in_elements "li"
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

  (* Make [%lwt] extension points part of the keywords they follow. *)
  let () =
    let extension = "%lwt" in
    let extension_length = String.length extension in
    soup $$ ".info > pre .keyword" |> iter begin fun keyword ->
      match next_sibling keyword with
      | Some sibling when not (is_element sibling) ->
        let sibling_text = texts sibling |> String.concat "" in
        let followed_by_extension =
          try String.sub sibling_text 0 extension_length = extension
          with Invalid_argument _ -> false
        in
        if not followed_by_extension then
          ()
        else
          let () =
            let new_keyword_text =
              texts keyword
              |> String.concat ""
              |> fun s -> s ^ extension
              |> create_text
            in
            clear keyword;
            append_child keyword new_keyword_text
          in
          let () =
            String.sub
              sibling_text
              extension_length
              (String.length sibling_text - extension_length)
            |> create_text
            |> replace sibling
          in
          ()

      | _ -> ()
    end
  in

  (* Get rid of table.typetable. *)
  let () =
    soup $$ ".typetable" |> iter begin fun table ->
      let type_name = table |> R.previous_element in
      table $$ "tr" |> iter begin fun row ->
        create_text "\n  | " |> append_child type_name;
        row $ "td:nth-child(2)"
        |> children
        |> to_list
        |> List.tl
        |>List.iter (append_child type_name)
      end;
      delete table
    end
  in

  (* Generate table of contents. *)
  let () =
    let toc = table_of_contents soup in
    let toc =
      toc |> List.map (fun (Section s) ->
        if s.name = "Deprecated" then
          Section {s with subsections = []}
        else
          (Section s))
    in
    let toc =
      (Section
        {name = "[Top]";
         anchor = "";
         subsections = []})::
      (Section
        {name = "Quick start";
         anchor = "3_Quickstart";
         subsections = []})::
      (Section
        {name = "Tutorial";
         anchor = "3_Tutorial";
         subsections = []})::
      (Section
        {name = "Execution model";
         anchor = "3_Executionmodel";
         subsections = []})::
      (Section
        {name = "Library guide";
         anchor = "3_GuidetotherestofLwt";
         subsections = []})::
      toc
    in

    let toc_div = create_element "div" ~class_:"toc" in

    toc
    |> markup_for_table_of_contents
    |> List.iter (fun e ->
      append_child toc_div (create_element "br");
      append_child toc_div e);

    toc_div $ "br" |> delete;

    create_element "h3" ~inner_text:"Table of contents" ~id:"toc"
    |> prepend_child toc_div;

    soup $ "h2" |> fun h2 -> insert_before h2 toc_div
  in

  (* Fix up long type signatures in wrapX functions. *)
  let () =
    soup $$ ".item[data-ml-identifier^='wrap']" |> iter begin fun wrap ->
      let name = R.attribute "data-ml-identifier" wrap in
      let arity =
        try int_of_string (String.sub name 4 1)
        with Invalid_argument _ -> 0
      in
      if arity < 1 then
        ()
      else
        let signature = wrap $ "code" in
        (* create_text "\n  " |> insert_before signature; *)
        let text_node = R.child signature in
        let text = R.leaf_text text_node in
        let left_paren = String.index text '(' in
        let right_paren = String.index text ')' in
        let argument =
          String.sub text (left_paren + 1) (right_paren - left_paren - 1) in
        let link = signature $ "a" in
        clear signature;
        fmt "\n  (%s) ⟶\n    (%s " argument argument
        |> create_text
        |> append_child signature;
        append_child signature link;
        create_text ")" |> append_child signature
    end
  in

  (* Line break for try_bind's long type signature. *)
  let () =
    let signature =
      soup $ ".item[data-ml-identifier='try_bind'] > pre > code" in
    create_text "\n  " |> insert_before signature
  in

  (* Be explicit about Lwt type names. *)
  let () =
    soup $$ ".item > pre a" |> iter begin fun link ->
      match R.leaf_text link with
      | "t" | "u" | "result" | "state" | "key" as text ->
        clear link;
        "Lwt." ^ text
        |> create_text
        |> append_child link
      | _ -> ()
    end
  in

  (* Create clickable anchors. *)
  let () =
    let for_element get_id element =
      let anchor = "#" ^ (get_id element) in
      let link =
        create_element
          ~class_:"anchor"
          ~attributes:["href", anchor]
          ~inner_text:"¶"
          "a"
      in
      prepend_child element link
    in

    soup $$ "h2" |> iter (for_element (fun h2 -> R.id h2));
    soup $$ "h3" |> iter (for_element (fun h3 -> R.id h3));
    soup $$ ".item > pre" |> iter (for_element (fun item ->
      item $ "span[id]" |> R.id));
    ()
  in

  (* Final dirty patches. *)
  let () =
    let wrap_first_argument_list element =
      let text_span = element $ "> pre > code" in
      let first = text_span |> children |> R.nth 1 in
      first
      |> R.leaf_text
      |> fun s -> "(" ^ s
      |> create_text
      |> replace first;
      let third = text_span |> children |> R.nth 3 in
      third
      |> R.leaf_text
      |> fun s -> ")" ^ s
      |> create_text
      |> replace third
    in
    soup $ ".item[data-ml-identifier=join]" |> wrap_first_argument_list;
    soup $ ".item[data-ml-identifier=pick]" |> wrap_first_argument_list;
    soup $ ".item[data-ml-identifier=choose]" |> wrap_first_argument_list;
    soup $ ".item[data-ml-identifier=npick]" |> wrap_first_argument_list;
    soup $ ".item[data-ml-identifier=nchoose]" |> wrap_first_argument_list;
    soup $ ".item[data-ml-identifier=nchoose_split]"
    |> wrap_first_argument_list;

    let replace_nth_text selector n replacement =
      create_text replacement
      |> replace (soup $ selector |> children |> R.nth n)
    in
    replace_nth_text
      ".item[data-ml-identifier=npick] > pre > code" 3 ") list ⟶ (α list) ";
    replace_nth_text
      ".item[data-ml-identifier=nchoose] > pre > code" 3 ") list ⟶ (α list) ";
    replace_nth_text
      ".item[data-ml-identifier=nchoose_split] > pre > code" 3
      ") list ⟶ (α list * [(α ";
    replace_nth_text
      ".item[data-ml-identifier=nchoose_split] > pre > code" 5
      ") list]) ";

    replace_nth_text
      ".item[data-ml-identifier=return_none] > pre > code" 1
      "(α option) ";
    replace_nth_text
      ".item[data-ml-identifier=return_nil] > pre > code" 1
      "(α list) ";

    replace_nth_text
      ".item[data-ml-identifier=add_task_r] > pre > code" 1
      "(α ";
    replace_nth_text
      ".item[data-ml-identifier=add_task_r] > pre > code" 3
      ") ";

    replace_nth_text
      ".item[data-ml-identifier=add_task_l] > pre > code" 1
      "(α ";
    replace_nth_text
      ".item[data-ml-identifier=add_task_l] > pre > code" 3
      ") ";

    replace_nth_text
      ".item[data-ml-identifier=return_some] > pre > code" 1
      "α ⟶ (α option) ";

    replace_nth_text
      ".item[data-ml-identifier=return_ok] > pre > code" 1
      "α ⟶ [(α, β) Result.result] ";
    replace_nth_text
      ".item[data-ml-identifier=return_error] > pre > code" 1
      "α ⟶ [(α, β) Result.result] ";
  in

  let () =
    let extract_annotations annotation paragraph =
      match paragraph $? (fmt "b:contains('%s')" annotation) with
      | None -> ()
      | Some annotation ->
        let new_paragraph = create_element "p" in
        (Obj.magic annotation)::(annotation |> next_siblings |> to_list)
        |> List.iter (append_child new_paragraph);
        insert_after paragraph new_paragraph
    in
    soup $ ".item[data-ml-identifier=return_ok] p"
    |> extract_annotations "Since";
    soup $ ".item[data-ml-identifier=return_error] p"
    |> extract_annotations "Since";
    soup $ ".item[data-ml-identifier=make_value] p"
    |> extract_annotations "Deprecated";
  in

  let () =
    let element =
      soup $ ".item[data-ml-identifier=result] > pre > span > code" in
    clear element;
    append_child element (create_text "+α")
  in

  (* Replace the <h1> element with a new header from header.html. *)
  read_file "docs/header.html" |> parse |> replace (soup $ "h1");

  (* Convert the soup back to HTML and write to STDOUT. The Makefile redirects
     that to the right output file. *)
  soup |> to_string |> write_channel stdout
