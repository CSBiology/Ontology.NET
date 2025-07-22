module internal ImportAux


open System.IO


module RegexPatterns =

    open System.Text.RegularExpressions

    let absoluteFilePathUnix = Regex("""^\s*[A-Za-z]:[\\/](?:(?![<>:""/\\|?*\r\n])[^. \r\n][^<>:""/\\|?*\r\n]*[^. \r\n]|[^. \r\n])(?:[\\/](?:(?![<>:""/\\|?*\r\n])[^. \r\n][^<>:""/\\|?*\r\n]*[^. \r\n]|[^. \r\n]))*$""")

    let relativeFilePath = Regex("""^\s*(?![A-Za-z]:[\\/])(?:[\\/])?(?:\.{1,2}|[^<>:""/\\|?*\r\n ][^<>:""/\\|?*\r\n]*[^. \r\n]|[^<>:""/\\|?*\r\n])(?:[\\/](?:\.{1,2}|[^<>:""/\\|?*\r\n ][^<>:""/\\|?*\r\n]*[^. \r\n]|[^<>:""/\\|?*\r\n]))*$""")

    let url = Regex(@"(?i)\b((?:https?|ftp)://[^\r\n\t""'<>()\[\]]+)")


type UriType =
    | AbsoluteFilePath of string
    | RelativeFilePath of string
    | Url of string


open RegexPatterns

let downloadString (uri : string) =
    let wc = new System.Net.WebClient()
    wc.DownloadString(uri)

let getOrReturnDir path =
    if File.GetAttributes(path).HasFlag(FileAttributes.Directory) then
        path
    else 
        (FileInfo path).Directory.FullName

let recognizeInput basePath input =
    match input with
    | x when (url.Match(x)).Success -> 
        Url input
    | x when (absoluteFilePathUnix.Match(x)).Success -> 
        AbsoluteFilePath input
    | x when (relativeFilePath.Match(x)).Success -> 
        RelativeFilePath input
    | _ ->
        raise (System.ArgumentException($"{input} is no known input of either absolute path, relative path, or URL."))