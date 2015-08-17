(*
 * ZPL F# Tool (https://github.com/janderit/ZPL.git)
 * Philip Jander, 2015
 *
 * This file and its project are licensed under the GNU LESSER GENERAL PUBLIC LICENSE v3 (LGPLv3).
 *)
namespace JanderIT

module ZPL =
    open System

    type ZPL_section =
        { Name : string
          Elements : ZPL_element array }

    and ZPL_KeyValue =
        { Key : string
          Value : string }

    and ZPL_element =
        | Section of ZPL_section
        | Value of ZPL_KeyValue

    and ZPL_set = ZPL_section array

    module internal Parser =
        let split_first_line (input : string) =
            let parts = input.Split([| "\r\n"; "\r"; "\n" |], 2, StringSplitOptions.None)

            let remainder =
                if parts.Length > 1 then parts.[1].TrimStart([| '\r'; '\n' |])
                else ""

            let next_line = parts.[0]
            next_line, remainder

        let rec slurp_comment_block input =
            let next_line, remainder = split_first_line input
            if (next_line.TrimEnd().EndsWith("*/")) then remainder
            else slurp_comment_block remainder

        let check_line_comment (strict : bool) (input : string) =
            let trimmed = input.Trim()
            trimmed.StartsWith("#") || (not strict && trimmed.StartsWith("//"))

        let check_block_comment (strict : bool) (input : string) =
            let trimmed = input.Trim()
            (not strict && trimmed.StartsWith("/*"))

        let rec get_line strict input =
            let next_line, remainder = split_first_line input
            let is_line_comment = check_line_comment strict next_line
            let is_block_comment = check_block_comment strict next_line
            if is_block_comment then slurp_comment_block input |> get_line strict
            else if is_line_comment then get_line strict remainder
            else next_line, remainder

        let get_indent (line : string) =
            System.Linq.Enumerable.Count(System.Linq.Enumerable.TakeWhile(line, (fun c -> c = ' '))) / 4

        let parse_key_value (input : string) =
            let parts = input.TrimStart().Split([| '=' |], 2)
            { Key = parts.[0]
              Value = parts.[1] }

        let rec parse_section_body indent strict input (elements : ZPL_element list) =
            let next_line, remainder = get_line strict input
            if (get_indent next_line) <> indent then elements, input
            else if next_line.Contains("=") then
                parse_section_body indent strict remainder (Value(parse_key_value next_line) :: elements)
            else
                let subsection, remainder = parse_section strict input
                parse_section_body indent strict remainder (Section(subsection) :: elements)

        and parse_section strict input : ZPL_section * string =
            let name, remainder = get_line strict input
            let indent = get_indent name
            let elements, remainder = parse_section_body (indent + 1) strict remainder []
            { Name = (name.Trim())
              Elements =
                  elements
                  |> Seq.toArray
                  |> Array.rev }, remainder

        and parse_set strict input concat_with : ZPL_set =
            let section, remaining = parse_section strict input
            if not (String.IsNullOrWhiteSpace(remaining)) then parse_set strict remaining (section :: concat_with)
            else
                (section :: concat_with)
                |> Seq.toArray
                |> Array.rev

    module internal Generator =
        let padding indent_level = "".PadLeft(4 * indent_level, ' ')

        let rec render_keyvalue (indent_level : int) (element : ZPL_KeyValue) =
            (padding indent_level) + element.Key + "=" + element.Value + Environment.NewLine

        and render_element (indent_level : int) (element : ZPL_element) =
            match element with
            | Value(e) -> render_keyvalue indent_level e
            | Section(e) -> render_section indent_level e

        and render_section (indent_level : int) (section : ZPL_section) =
            (padding indent_level) + section.Name + Environment.NewLine
            + (section.Elements |> Seq.fold (fun a b -> a + (render_element (indent_level + 1) b)) "")

    let kv (key : string) (value : string) : ZPL_element =
        Value({ Key = key
                Value = value })

    let top_section (name : string) ([<ParamArray>] elements : ZPL_element array) : ZPL_section =
        { Name = name
          Elements = elements }

    let section (name : string) ([<ParamArray>] elements : ZPL_element array) : ZPL_element =
        Section(top_section name elements)
    let create ([<ParamArray>] sections : ZPL_section array) : ZPL_set = sections
    let render (set : ZPL_set) : string = String.Join("", set |> Seq.map (Generator.render_section 0))
    let parse input : ZPL_set = Parser.parse_set true input []
    let parse_relaxed input : ZPL_set = Parser.parse_set false input []

    module API =
        let top_section (name : string) (elements : ZPL_element seq) = top_section name <| Seq.toArray elements
        let section (name : string) (elements : ZPL_element seq) = section name <| Seq.toArray elements
        let kv (key : string) (value : string) = kv key value

        let single (name : string) (elements : ZPL_element seq) =
            [| top_section name elements |]
            |> create
            |> render

        let to_zpl (sections : ZPL_section seq) =
            Seq.toArray sections
            |> create
            |> render

        let is_section (element : ZPL_element) : bool =
            match element with
            | Section(_) -> true
            | _ -> false

        let unpack_section (Section(section)) = section

        let is_keyvalue (element : ZPL_element) : bool =
            match element with
            | Value(_) -> true
            | _ -> false

        let unpack_keyvalue (Value(kv)) = kv

        let find_sections
            (name : string)
            (zpl : ZPL_element seq)
            : ZPL_section list
            =
            zpl
            |> Seq.filter is_section
            |> Seq.map unpack_section
            |> Seq.filter (fun section -> section.Name = name)
            |> Seq.toList

        let find_section name zpl =
            match (find_sections name zpl) with
            | [] -> None
            | s::_ -> Some(s)

        let find_section_or_empty name zpl =
            match (find_section name zpl) with
            | None -> top_section name Seq.empty
            | Some(s) -> s

        let find_values
            (name : string)
            (zpl : ZPL_element seq)
            : string list
            =
            zpl
            |> Seq.filter is_keyvalue
            |> Seq.map unpack_keyvalue
            |> Seq.filter (fun kv -> kv.Key = name)
            |> Seq.map (fun kv -> kv.Value)
            |> Seq.toList

        let find_value name zpl =
            match (find_values name zpl) with
            | [] -> None
            | s::_ -> Some(s)

        let find_value_or name defaultto zpl =
            match (find_value name zpl) with
            | None -> defaultto
            | Some(s) -> s

