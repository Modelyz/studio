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
async def home(request: Request):
    return templates.TemplateResponse(
        "index.html",
        {"request": request})


# home page
@app.get("/{a}", response_class=HTMLResponse)
async def one_level(request: Request, a: str):
    return templates.TemplateResponse(
        "index.html",
        {"request": request, 'a': a})


# home page
@app.get("/{a}/{b}", response_class=HTMLResponse)
async def two_levels(request: Request, a: str, b: str):
    return templates.TemplateResponse(
        "index.html",
        {"request": request, 'a': a, 'b': b})
