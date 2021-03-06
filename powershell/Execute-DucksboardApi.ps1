function Execute-DucksboardApi
{
  param(
    [string] $url = $null,
    [string] $data = $null,
    [string] $apikey = $null,
    [string] $contentType = "application/json",
    [string] $codePageName = "UTF-8",
    [string] $userAgent = $null
  );

  if ($url -and $data -and $apikey)
  {
    [System.Net.WebRequest]$webRequest = [System.Net.WebRequest]::Create($url);
    $webRequest.ServicePoint.Expect100Continue = $false;
    [System.Net.NetworkCredential]$credentials = New-Object System.Net.NetworkCredential($apikey, 'ignored');
    $webRequest.Credentials = $credentials.GetCredential($url, 'Basic');
    $webRequest.PreAuthenticate = $true;
    $webRequest.ContentType = $contentType;
    $webRequest.Method = "POST";
    if ( $userAgent )
    {
      $webRequest.UserAgent = $userAgent;
    }

    $enc = [System.Text.Encoding]::GetEncoding($codePageName);
    [byte[]]$bytes = $enc.GetBytes($data);
    $webRequest.ContentLength = $bytes.Length;
    [System.IO.Stream]$reqStream = $webRequest.GetRequestStream();
    $reqStream.Write($bytes, 0, $bytes.Length);
    $reqStream.Flush();

    $resp = $webRequest.GetResponse();
    $rs = $resp.GetResponseStream();
    [System.IO.StreamReader]$sr = New-Object System.IO.StreamReader -argumentList $rs;
    $sr.ReadToEnd();
  }
}
