from selenium import webdriver
from selenium.webdriver.chrome.service import Service as ChromeService
from selenium.webdriver.common.by import By
from selenium.webdriver.firefox.service import Service as FirefoxService
from selenium.webdriver.support.wait import WebDriverWait
from webdriver_manager.chrome import ChromeDriverManager
from webdriver_manager.firefox import GeckoDriverManager
import os
import subprocess
import time

ES = "messagestore.txt"
site = "http://localhost:8080"

is_running = lambda proc: proc.poll() is None


def backends():
    plist = []
    if os.path.exists(ES):
        os.remove(ES)
    with subprocess.Popen(["../../build/studio", "-d", "../../build/"]) as proc:
        plist.append(proc)
        yield plist
        for p in plist:
            p.terminate()
        for p in plist:
            p.wait()


def driver(nav):
    if nav == "chrome":
        service = ChromeService(executable_path=ChromeDriverManager().install())
        driver = webdriver.Chrome(service=service)
    elif nav == "firefox":
        service = FirefoxService(executable_path=GeckoDriverManager().install())
        driver = webdriver.Firefox(service=service)
    else:
        raise NotImplementedError
    return driver


def count_evstore(string):
    with open(ES) as es:
        return es.read().count(string)


def click(browser, text):
    time.sleep(0.05)  # couldn't find a way to wait a detectable change
    print("### click on " + text)
    # WebDriverWait(browser, timeout=1).until(
    #    cond.presence_of_all_elements_located((By.XPATH, f"//*[text()='{text}']"))
    # )
    text = text.replace("'", "\\'")
    return (
        WebDriverWait(browser, timeout=1)
        .until(lambda d: d.find_element(By.XPATH, f"//*[text()='{text}']"))
        .click()
    )


def fill(browser, text):
    return (
        WebDriverWait(browser, timeout=1)
        .until(lambda d: d.find_element(By.TAG_NAME, "input"))
        .send_keys(text)
    )


def fill_by_id(browser, id_, text):
    return (
        WebDriverWait(browser, timeout=1)
        .until(lambda d: d.find_element(By.ID, id_))
        .send_keys(text)
    )


def find_by_id(browser, text):
    return browser.find_element(By.ID, text)


def find_by_text(browser, text):
    text = text.replace("'", "\'")
    return browser.find_element(By.XPATH, f"//*[contains(text(),'{text}')]")


def text_exists(browser, text):
    assert (
        find_by_text(browser, text).text == text
    ), f"The text '{text}' could not be found"


def add_identifier_type(browser, name, scope=[], options={}, fragments=[]):
    open_url(browser, f"/identifier-type/add")
    for s in scope:
        click(browser, s)
    click(browser, "Next →")
    fill(browser, name)
    click(browser, "Next →")
    for o, doClick in options.items():
        if doClick:
            click(browser, o)
    click(browser, "Next →")
    for _, f in enumerate(fragments):
        click(browser, f[0] if type(f) is tuple else f)
    for i, f in enumerate(fragments):
        if type(f) is tuple:
            if f[0] == "Fixed":
                fill_by_id(browser, "segment/" + str(i), f[1])
    click(browser, "Validate and finish")

def add_value_type(browser, name, scope=[], options={}, expression=[], fields={}):
    open_url(browser, f"/value-type/add")
    for s in scope:
        click(browser, s)
    click(browser, "Next →")
    fill(browser, name)
    click(browser, "Next →")
    for o, doClick in options.items():
        if doClick:
            click(browser, o)
    click(browser, "Next →")
    for e in expression:
        click(browser, e)
    for x, y in fields.items():
        fill_by_id(browser, x, y)
    click(browser, "Validate and finish")


def add_zone(browser, zone="", scope=[], fragments=[]):
    open_url(browser, "/configuration/add")
    click(browser, zone)
    for s in scope:
        click(browser, s)
    for _, f in enumerate(fragments):
        click(browser, f[0] if type(f) is tuple else f)
    for i, f in enumerate(fragments):
        if type(f) is tuple:
            if f[0] == "Fixed":
                fill_by_id(browser, "segment/" + str(i), f[1])
    click(browser, "Validate and finish")


def add_any_type(browser, vsegment, identifiers={}, values={}, options={}):
    open_url(browser, f"/{vsegment}/add")
    click(browser, "Next →")
    for name, value in identifiers.items():
        print(identifiers)
        fill_by_id(browser, name, value)
    click(browser, "Next →")
    for name, value in values.items():
        fill_by_id(browser, name, value)
    click(browser, "Next →")
    for o, doClick in options.items():
        if doClick:
            click(browser, o)
    click(browser, "Next →")
    click(browser, "Validate and finish")


def add_group_type(
    browser,
    types=[],
    identifiers={},
    single=False,
    tree="Not",
    values={},
    options={},
):
    assert tree in (
        "Not hierarchical",
        "Hierarchical, an intermediate node can be selected",
        "Hierarchical, a leaf must be selected",
    )
    open_url(browser, "/group-type/add")
    for p in types:
        click(browser, p)
    click(browser, "Next →")
    for name, value in identifiers.items():
        fill_by_id(browser, name, value)
    click(browser, "Next →")
    if not single:
        click(browser, str(single))
    click(browser, "Next →")
    click(browser, tree)
    click(browser, "Next →")
    for name, value in values.items():
        fill_by_id(browser, name, value)
    click(browser, "Next →")
    for o, doClick in options.items():
        if doClick:
            click(browser, o)
    click(browser, "Next →")
    click(browser, "Validate and finish")


def add_event_type(
    browser,
    identifiers={},
    providers=[],
    receivers=[],
    flow=[],
    expression=[],
    fields={},
    options={},
):
    open_url(browser, "/event-type/add")
    click(browser, "Next →")
    for name, value in identifiers.items():
        fill_by_id(browser, name, value)
    click(browser, "Next →")
    click(browser, "Next →")
    for s in providers:
        click(browser, s)
    click(browser, "Next →")
    for s in receivers:
        click(browser, s)
    click(browser, "Next →")
    for s in flow:
        click(browser, s)
    for e in expression:
        click(browser, e)
    for x, y in fields.items():
        fill_by_id(browser, x, y)
    click(browser, "Next →")
    for o, doClick in options.items():
        if doClick:
            click(browser, o)
    click(browser, "Next →")
    click(browser, "Validate and finish")


def add_process_type(browser, types=[], identifiers={}, eventTypes=[], options={}):
    open_url(browser, "/process-type/add")
    for t in types:
        click(browser, t)
    click(browser, "Next →")
    for name, value in identifiers.items():
        fill_by_id(browser, name, value)
    click(browser, "Next →")
    for e in eventTypes:
        click(browser, e)
    click(browser, "Next →")
    click(browser, "Next →")
    for o, doClick in options.items():
        if doClick:
            click(browser, o)
    click(browser, "Next →")
    click(browser, "Validate and finish")


def add_entity(browser, vsegment, types=[], identifiers={}, values={}, groups={}):
    open_url(browser, f"/{vsegment}/add")
    for t in types:
        click(browser, t)
    click(browser, "Next →")
    for name, value in identifiers.items():
        fill_by_id(browser, name, value)
    click(browser, "Next →")
    for name, value in values.items():
        fill_by_id(browser, name, value)
    click(browser, "Next →")
    for groupType, groups in groups.items():
        click(browser, groupType)
        for group in groups:
            click(browser, group)
    click(browser, "Validate and finish")


def open_url(browser, path):
    print("### open " + path)
    browser.get(site + path)
