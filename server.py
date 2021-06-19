from fastapi import FastAPI, Request
from fastapi.staticfiles import StaticFiles
from fastapi.responses import HTMLResponse
from fastapi.templating import Jinja2Templates

app = FastAPI()
templates = Jinja2Templates(directory="templates")


# static folder
app.mount("/static", StaticFiles(directory="static"), name="static")


# home page
@app.get("/", response_class=HTMLResponse)
async def read_item(request: Request):
    return templates.TemplateResponse(
        "index.html",
        {"request": request})


# home page
@app.get("/{a}/{b}", response_class=HTMLResponse)
async def read_item2(request: Request, a: str, b: int):
    return templates.TemplateResponse(
        "index.html",
        {"request": request, 'a': a, 'b': b})
