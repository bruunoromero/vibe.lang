export const httpGet = (url) => {
  return fetch(url).then(async (res) => {
    const body = await res.text();
    return { status: res.status, body };
  });
};

export const httpPost = (url, body) => {
  return fetch(url, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body,
  }).then(async (res) => {
    const responseBody = await res.text();
    return { status: res.status, body: responseBody };
  });
};

export const responseStatus = (r) => r.status;

export const responseBody = (r) => r.body;
