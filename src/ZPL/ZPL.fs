﻿(*
* ZPL F# Tool (https://github.com/ASD-GmbH/ZPL)
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
      let is_comment strict (line : String) =
        line.StartsWith "#" || (not strict && line.StartsWith "//")

      let is_begin_of_comment_block (line : String) =
        line.StartsWith "/*"

      let is_end_of_comment_block (line : String) =
        line.EndsWith "*/"

      let evaluate_line strict (in_comment_block, lines) (line : string) : bool * string list =
        let trimmedLine = line.Trim()
        if in_comment_block then
          (not (trimmedLine |> is_end_of_comment_block), lines)
        else if trimmedLine |> is_comment strict then
          (in_comment_block, lines)
        else if not strict && trimmedLine |> is_begin_of_comment_block then
          (true, lines)
        else
          (in_comment_block, line::lines)

      let lines_without_comments strict (input : string) =
        input.Split([| "\r\n"; "\r"; "\n" |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.fold (evaluate_line strict) (false, [])
        |> fun (_, lines) -> lines
        |> List.rev

      let indentation_of (line : string) =
        line
        |> Seq.takeWhile (fun c -> c = ' ')
        |> Seq.length
        |> fun lenght -> lenght / 4
        |> int

      type Parsing_Element =
        | ParsedSection of Parsing_Section
        | ParsedValue of ZPL_KeyValue

      and Parsing_Section =
        { name: string
          elements : Parsing_Element list
        }

      type Parsing_current_Section =
        | Root of Parsing_Section
        | Nested of level:int * Parsing_Section * Parsing_current_Section

      let tryParseKeyValue (input : string) : ZPL_KeyValue option =
        let parts = input.TrimStart().Split([| '=' |], 2)
        if parts.Length = 2 then
          Some { Key = parts.[0] ; Value = parts.[1] }
        else
          None

      let section_from_line (name : string) : Parsing_Section =
        { name = name.Trim()
          elements = [] }

      let with_sub_section (sub : Parsing_Section) (section : Parsing_Section) =
        { section with elements = ParsedSection sub::section.elements }

      let rec resolve_section until_level (section : Parsing_Section) (parent : Parsing_current_Section) : (Parsing_Section * Parsing_current_Section option) =
        match parent with
        | Root (parentSection) ->
            parentSection |> with_sub_section section, None

        | Nested (level, parentSection, parentValue) ->
            let next_Section = parentSection |> with_sub_section section
            if level = until_level then
              next_Section, Some parentValue
            else
              resolve_section until_level next_Section parentValue

      let element_into (section : Parsing_Section) (element : ZPL_KeyValue) =
        { section with elements = ParsedValue element::section.elements }

      let parseLine (into : Parsing_Section -> Parsing_current_Section) (section : Parsing_Section) (line : String) : Parsing_current_Section =
        tryParseKeyValue line
        |> Option.map (element_into section)
        |> Option.map into
        |> Option.defaultValue (Nested (1, section_from_line line, into section))

      let evaluate_element (current, set) (line : string) : (Parsing_current_Section * Parsing_Section list) =
        match current with
        | Root section ->
            (parseLine Root section line, set)

        | Nested (indentation, section, parent) ->
            let current_indentation = indentation_of line

            let (next,nextParent) =
              if current_indentation < indentation then
                resolve_section current_indentation section parent
              else
                (section, Some parent)

            nextParent
            |> Option.map (fun p -> (parseLine (fun s -> Nested (current_indentation, s, p)) next line), set)
            |> Option.defaultValue (Root (section_from_line line), next::set)

      let rec section_of (value : Parsing_current_Section) : Parsing_Section =
        match value with
        | Root section -> section
        | Nested (_, section, parent) ->
            section_of parent
            |> with_sub_section section

      let rec as_ZPL_section (value : Parsing_Section) : ZPL_section =
        { Name = value.name
          Elements =
            value.elements
            |> List.map (function | ParsedSection s -> s |> as_ZPL_section |> Section | ParsedValue v -> Value v)
            |> List.rev
            |> List.toArray
        }

      let parse_set strict (input : string): ZPL_set =
        match input |> lines_without_comments strict with
        | [] ->
            [||]

        | [line] ->
            [| section_from_line line |> as_ZPL_section |]

        | line::rest ->
            rest
            |> List.fold evaluate_element (Root (section_from_line line), [])
            |> (fun (last, set) -> section_of last::set)
            |> List.map as_ZPL_section
            |> List.rev
            |> List.toArray

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
    let parse input : ZPL_set = Parser.parse_set true input
    let parse_relaxed input = Parser.parse_set false input

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

