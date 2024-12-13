type block =
  { id : int
  ; pos : int
  ; len : int
  }

type block_list_node =
  { mutable prev : block_list_node option
  ; mutable next : block_list_node option
  ; mutable element : block
  }

type block_list =
  { mutable head : block_list_node option
  ; mutable tail : block_list_node option
  }

let bl_insert_after elm node list =
  let new_node = { prev = Some node; next = node.next; element = elm } in
  (match node.next with
   | None -> list.tail <- Some new_node
   | Some next_node -> next_node.prev <- Some new_node);
  if Option.is_none list.head then list.head <- Some new_node;
  node.next <- Some new_node
;;

let bl_append elm list =
  match list.tail with
  | None ->
    let new_node = { prev = list.tail; next = None; element = elm } in
    list.head <- Some new_node;
    list.tail <- Some new_node
  | Some tail_node -> bl_insert_after elm tail_node list
;;

let make_block_list seq =
  let rec aux idx pos acc seq =
    match Seq.uncons seq with
    | None -> acc
    | Some (len, tl) ->
      if len > 0
      then (
        let id = if idx mod 2 = 0 then idx / 2 else -1 in
        bl_append { id; pos; len } acc;
        aux (idx + 1) (pos + len) acc tl)
      else aux (idx + 1) pos acc tl
  in
  aux 0 0 { head = None; tail = None } seq
;;

let split_files_to_len_one l =
  let rec aux node list =
    if node.element.len > 1 && node.element.id >= 0
    then (
      let id, pos, len = node.element.id, node.element.pos, node.element.len in
      bl_insert_after { id; pos = pos + 1; len = len - 1 } node list;
      node.element <- { node.element with len = 1 };
      aux (Option.get node.next) list)
    else (
      match node.next with
      | Some next_node -> aux next_node l
      | None -> l)
  in
  match l.head with
  | Some hd -> aux hd l
  | None -> l
;;

let defragment l =
  let rec find_next_free = function
    | Some node -> if node.element.id < 0 then Some node else find_next_free node.next
    | None -> None
  in
  let rec defragment' first_free f b list =
    match f, b with
    | None, _ -> list
    | _, None -> list
    | Some front, Some back ->
      if back.element.pos <= front.element.pos
      then defragment' first_free first_free back.prev list
      else if back.element.id < 0
      then defragment' first_free (Some front) back.prev list
      else if front.element.len < back.element.len
      then defragment' first_free (find_next_free front.next) (Some back) list
      else if front.element.len = back.element.len
      then (
        front.element <- { front.element with id = back.element.id };
        back.element <- { back.element with id = -1 };
        let ffree = find_next_free first_free in
        defragment' ffree ffree back.prev list)
      else (
        let _, fpos, flen = front.element.id, front.element.pos, front.element.len in
        let bid, _, blen = back.element.id, back.element.pos, back.element.len in
        bl_insert_after { id = -1; pos = fpos + blen; len = flen - blen } front list;
        front.element <- { front.element with id = bid; len = blen };
        back.element <- { back.element with id = -1 };
        let ffree = find_next_free first_free in
        defragment' ffree ffree back.prev list)
  in
  let ffree = find_next_free l.head in
  defragment' ffree ffree l.tail l
;;

let checksum l =
  let rec checksum_node acc id pos len =
    if len <= 0 then acc else checksum_node (acc + (pos * id)) id (pos + 1) (len - 1)
  in
  let rec checksum' acc = function
    | Some node ->
      let id = if node.element.id >= 0 then node.element.id else 0 in
      checksum' (checksum_node acc id node.element.pos node.element.len) node.next
    | None -> acc
  in
  checksum' 0 l.head
;;

let parse input =
  String.split_on_char '\n' input
  |> List.hd
  |> String.to_seq
  |> Seq.map (fun c -> Char.code c - Char.code '0')
  |> make_block_list
;;

let run input =
  [ checksum (defragment (split_files_to_len_one (parse input)))
  ; checksum (defragment (parse input))
  ]
;;

let example () = run "2333133121414131402"
