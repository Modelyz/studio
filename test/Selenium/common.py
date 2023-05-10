from selenium import webdriver
from selenium.webdriver.support import expected_conditions as econd
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
WAIT = 0.05
LASTSTEP_FILE = "resume"  # store the nb of function execution
LASTSTEP = int(open(LASTSTEP_FILE).read()) if os.path.exists(LASTSTEP_FILE) else 0
CURRENT = 0
TIMEOUT = 600

is_running = lambda proc: proc.poll() is None

# wait = selenium.WebDriverWait(chrome, timeout=10, poll_frequency=1)


def resumable(f):
    """decorator to allow to resume a failed run"""

    def resume(*args, **kw):
        global CURRENT, LASTSTEP
        if CURRENT > LASTSTEP:
            os.unlink(LASTSTEP_FILE)
            raise Exception("Invalid resume file deleted. Please restart")
        if CURRENT == LASTSTEP:
            try:
                result = f(*args, **kw)
            except:
                with open(LASTSTEP_FILE, "w") as fnb:
                    fnb.write(str(CURRENT))
                raise
            CURRENT += 1
            LASTSTEP = CURRENT
            return result
        else:
            print(f"skipping nb {CURRENT}")
            CURRENT += 1

    return resume


def wait(more=0.0):
    global WAIT
    time.sleep(WAIT + more)
    WAIT = WAIT  # + 0.0003


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


def open_url(browser, path):
    print("### open " + path)
    wait()
    browser.get(site + path)
    wait(0.1)


def click(browser, text, id_=None):
    print(f"### click on '{text}'")
    # WebDriverWait(browser, timeout=TIMEOUT).until(
    #    cond.presence_of_all_elements_located((By.XPATH, f"//*[text()='{text}']"))
    # )
    wait()  # couldn't find a way to wait a detectable change
    id_ = f" and id='{id_}'" if id_ else ""
    WebDriverWait(browser, timeout=TIMEOUT).until(
        econd.presence_of_element_located((By.XPATH, f"//*[text()='{text}'{id_}]"))
    )
    WebDriverWait(browser, timeout=TIMEOUT).until(
        econd.element_to_be_clickable((By.XPATH, f"//*[text()='{text}'{id_}]"))
    )
    WebDriverWait(browser, timeout=TIMEOUT).until(
        lambda d: d.find_element(By.XPATH, f"//*[text()='{text}'{id_}]")
    ).click()
    wait()  # couldn't find a way to wait a detectable change


def fill(browser, text):
    print(f"### input '{text}'")
    wait()  # couldn't find a way to wait a detectable change
    WebDriverWait(browser, timeout=TIMEOUT).until(
        econd.presence_of_element_located((By.TAG_NAME, "input"))
    )
    WebDriverWait(browser, timeout=TIMEOUT).until(
        lambda d: d.find_element(By.TAG_NAME, "input")
    ).send_keys(text)
    wait()  # couldn't find a way to wait a detectable change


def fill_by_id(browser, id_, text):
    print(f"### input '{text}' on id {id_}")
    wait()  # couldn't find a way to wait a detectable change
    WebDriverWait(browser, timeout=TIMEOUT).until(
        econd.presence_of_element_located((By.ID, id_))
    )
    WebDriverWait(browser, timeout=TIMEOUT).until(
        lambda d: d.find_element(By.ID, id_)
    ).send_keys(text)
    wait()  # couldn't find a way to wait a detectable change


def text_exists(browser, text):
    WebDriverWait(browser, timeout=TIMEOUT).until(
        econd.presence_of_element_located((By.XPATH, f'//*[contains(text(),"{text}")]'))
    )
    assert (
        browser.find_element(By.XPATH, f'//*[contains(text(),"{text}")]').text == text
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
    parentType="",
    identifiers={},
    single=None,
    tree="Not hierarchical",
    values={},
    options={},
):
    assert tree in (
        "Not hierarchical",
        "Hierarchical, an intermediate node can be selected",
        "Hierarchical, a leaf must be selected",
    )
    open_url(browser, "/group-type/add")
    if parentType:
        click(browser, parentType)
    click(browser, "Next →")
    for name, value in identifiers.items():
        fill_by_id(browser, name, value)
    click(browser, "Next →")
    if single:
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


def add_process_type(browser, parentType="", identifiers={}, eventTypes=[], options={}):
    open_url(browser, "/process-type/add")
    if parentType:
        click(browser, parentType)
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


def add_entity(browser, vsegment, parentType="", identifiers={}, values={}, groups={}):
    open_url(browser, f"/{vsegment}/add")
    if parentType:
        click(browser, parentType)
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


def add_group(
    browser, type_, contains=[], identifiers={}, parent="", values={}, groups={}
):
    open_url(browser, f"/group/add")
    click(browser, type_)
    click(browser, "Next →")
    for scope in contains:
        click(browser, scope)
    click(browser, "Next →")
    for name, value in identifiers.items():
        fill_by_id(browser, name, value)
    click(browser, "Next →")
    if parent:
        click(browser, parent)
    click(browser, "Next →")
    for name, value in values.items():
        fill_by_id(browser, name, value)
    click(browser, "Next →")
    for group in groups.items():
        click(browser, group[0])
        for group in group[1]:
            click(browser, group)
    click(browser, "Validate and finish")
