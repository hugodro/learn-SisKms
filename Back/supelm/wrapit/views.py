import traceback
import sys
import time

from django.shortcuts import render

from rest_framework.response import Response
from rest_framework import status
from rest_framework.decorators import api_view
from rest_framework.views import APIView
from rest_framework.parsers import MultiPartParser, FormParser

# Create your views here.


def getOptions():
  return [
      { "id": 1, "label": "Elm Test, SIS KMS", "function": test_1 }
    ]



def landingPage(request):
  """Main landing page for the project"""
  page= "wrapit/landing.html"
  pageCtxt= { "options": getOptions }
  response= render(request, page, { "pCtxt" : pageCtxt })
  response["Access-Control-Allow-Origin"] = "http://chodov:7001"
  response["Access-Control-Allow-Methods"] = "GET, POST, OPTIONS"
  response["Access-Control-Max-Age"] = "1000"
  #response["Access-Control-Allow-Headers"] = "X-Requested-With, Content-Type"
  return response


def testDispatcher(request):
  try:
    tID= int(request.GET.get("a", 0))
    if tID > 0 and tID < len(getOptions())+1:
      page, pageCtxt= getOptions()[tID - 1]["function"](request)
    else:
      #TODO: handle an invalid test id.
      pass
  except Exception as e:
    print("@[testDispatcher] excp %s" % (e))
    traceback.print_exc(file=sys.stdout)
    page= "wrapit/error.html"
    pageCtxt= { "error": repr(e) }
  response= render(request, page, { "pCtxt": pageCtxt})
  response["Access-Control-Allow-Origin"] = "http://chodov:7001"
  response["Access-Control-Allow-Methods"] = "POST, OPTIONS"
  response["Access-Control-Max-Age"] = "1000"
  response["Access-Control-Allow-Headers"] = "X-Requested-With, Content-Type"
  return response


@api_view(["POST"])
def apiStub(request):
  try:
    tID= int(request.POST.get("a", 0))
    print("@[apiStub] got a: %d" % (tID))
    items= { "result": "ok"
         , "items": {
           "english": [
              ( "Mon", "engMon_200504", "2020-05-04T00:00:00Z")
              , ("Tue", "engTue_200505", "2020-05-05T00:00:00Z")
              , ( "Wed", "engWed_200506", "2020-05-06T00:00:00Z")
              , ( "Thu", "engThu_200507", "2020-05-07T00:00:00Z")
              , ( "Mon", "engMon_200511", "2020-05-11T00:00:00Z")
              , ( "Tue", "engTue_200512", "2020-05-12T00:00:00Z")
              , ( "Wed", "engWed_200513", "2020-05-13T00:00:00Z")
              , ( "Thu", "engY1Thu_200514", "2020-05-14T00:00:00Z")
              , ( "Mon", "engMon_200518", "2020-05-18T00:00:00Z")
              , ( "Tue", "engTue_200519", "2020-05-19T00:00:00Z")
              , ( "Wed", "engY1Wed_200520", "2020-05-20T00:00:00Z")
              , ( "Thu", "engY1Thu_200521", "2020-05-21T00:00:00Z")
              , ( "Tue", "engTue_200526", "2020-05-26T00:00:00Z")
              , ( "Wed", "engWed_200527", "2020-05-27T00:00:00Z")
              , ( "Thu", "engThu_200528", "2020-05-28T00:00:00Z")
              , ( "Mon", "engMon_200601", "2020-06-01T00:00:00Z")
              , ( "Tue", "engTue_200602", "2020-0602T00:00:00Z")
              , ( "Wed", "engWed_200603", "2020-06-03T00:00:00Z")
              , ( "Thu", "engThu_200604", "2020-06-04T00:00:00Z")
              ]
            , "ipc": [
               ( "Mon", "ipcMon_200504", "2020-05-04T00:00:00Z")
              , ( "Tue", "ipcTue_200505", "2020-05-05T00:00:00Z")
              , ( "Wed", "ipcWed_200506", "2020-05-06T00:00:00Z")
              , ( "Thu", "ipcThu_200507", "2020-05-07T00:00:00Z")
              , ( "Fri", "ipcFri_200508", "2020-05-08T00:00:00Z")
              , ( "Mon", "ipcMon_200511", "2020-05-11T00:00:00Z")
              , ( "Tue", "ipcTue_200512", "2020-05-12T00:00:00Z")
              , ( "Wed", "ipcWed_200513", "2020-05-13T00:00:00Z")
              , ( "Thu", "ipcThu_200514", "2020-05-14T00:00:00Z")
              , ( "Fri", "ipcFri_200515", "2020-05-15T00:00:00Z")
              , ( "Mon", "ipcMon_200518", "2020-05-18T00:00:00Z")
              , ( "Tue", "ipcTue_200519", "2020-05-19T00:00:00Z")
              , ( "Wed", "ipcWed_200520", "2020-05-20T00:00:00Z")
              , ( "Thu", "ipcThu_200521", "2020-05-21T00:00:00Z")
              , ( "Fri", "ipcFri_200522", "2020-05-22T00:00:00Z")
              , ( "Tue", "ipcTue_200526", "2020-05-26T00:00:00Z")
              , ( "Wed", "ipcWed_200527", "2020-05-27T00:00:00Z")
              , ( "Thu", "ipcThu_200528", "2020-05-28T00:00:00Z")
              , ( "Fri", "ipcFri_200529", "2020-05-29T00:00:00Z")
              , ( "Mon", "ipcMon_200601", "2020-06-01T00:00:00Z")
              , ( "Tue", "ipcTue_200602", "2020-06-02T00:00:00Z")
              , ( "Wed", "ipcWed_200603", "2020-06-03T00:00:00Z")
              , ( "Thu", "ipcThu_200604", "2020-06-04T00:00:00Z")
              , ( "Fri", "ipcFri_200605", "2020-06-05T00:00:00Z")
              ]
            , "math": [
              ( "Mon", "mathMon_200504", "2020-05-04T00:00:00Z")
              , ( "Tue", "mathTue_200505", "2020-05-05T00:00:00Z")
              , ( "Wed", "mathWed_200506", "2020-05-06T00:00:00Z")
              , ( "Thu", "mathThu_200507", "2020-05-07T00:00:00Z")
              , ( "Mon", "mathY1Mon_200511", "2020-05-11T00:00:00Z")
              , ( "Tue", "mathY1Tue_200512", "2020-05-12T00:00:00Z")
              , ( "Wed", "mathY1Wed_200513", "2020-05-13T00:00:00Z")
              , ( "Mon", "mathY1Mon_200518", "2020-05-18T00:00:00Z")
              , ( "Tue", "mathY1Tue_200519", "2020-05-19T00:00:00Z")
              , ( "Wed", "mathY1Wed_200520", "2020-05-20T00:00:00Z")
              , ( "Thu", "mathY1_200521", "2020-05-21T00:00:00Z")
              , ( "Tue", "mathY1Tue_200526", "2020-05-26T00:00:00Z")
              , ( "Wed", "mathWed_200527", "2020-05-27T00:00:00Z")
              , ( "Thu", "mathThu_200528", "2020-05-28T00:00:00Z")
              , ( "Mon", "mathMon_20601", "2020-06-01T00:00:00Z")
              , ( "Tue", "mathTue_200602", "2020-06-02T00:00:00Z")
              , ( "Wed", "mathWed_200603", "2020-06-03T00:00:00Z")
              , ( "Thu", "mathThu_200604", "2020-06-04T00:00:00Z")
              ]
            , "phonics": [
              ( "Mon", "phseMon_200504", "2020-05-04T00:00:00Z")
              , ( "Tue", "phseTue_200505", "2020-05-05T00:00:00Z")
              , ( "Wed", "phseWed_200506", "2020-05-06T00:00:00Z")
              , ( "Thu", "phseThu_200507", "2020-05-07T00:00:00Z")
              , ( "Fri", "phseFri_200508", "2020-05-08T00:00:00Z")
              , ( "Mon", "phseMon_200511", "2020-05-11T00:00:00Z")
              , ( "Tue", "phseTue_200512", "2020-05-12T00:00:00Z")
              , ( "Wed", "phseWed_200513", "2020-05-13T00:00:00Z")
              , ( "Thu", "phseThu_200514", "2020-05-14T00:00:00Z")
              , ( "Fri", "phseCaringFri_200515", "2020-05-15T00:00:00Z")
              , ( "Fri", "phseFri_200515", "2020-05-15T00:00:00Z")
              , ( "Mon", "phseMon_200518", "2020-05-18T00:00:00Z")
              , ( "Tue", "phseTue_200519", "2020-05-19T00:00:00Z")
              , ( "Wed", "phseWed_200520", "2020-05-20T00:00:00Z")
              , ( "Fri", "phseFri_200522", "2020-05-22T00:00:00Z")
              , ( "Tue", "phseTue_200526", "2020-05-26T00:00:00Z")
              , ( "Thu", "phseThu_200528", "2020-05-28T00:00:00Z")
              , ( "Fri", "phseFri_Exc_200529", "2020-05-29T00:00:00Z")
              , ( "Fri", "phseFri_200529", "2020-05-29T00:00:00Z")
              , ( "Mon", "phseMon_200601", "2020-06-01T00:00:00Z")
              , ( "Tue", "phseTue_200602", "2020-06-02T00:00:00Z")
              , ( "Wed", "phseWed_200603", "2020-06-03T00:00:00Z")
              , ( "Thu", "phseThu_200604", "2020-06-04T00:00:00Z")
              , ( "Fri", "phseFri_200605", "2020-06-05T00:00:00Z")
              ]
          }
    }
  except Exception as e:
    print("@[apiStub] excp %s" % (e))
    traceback.print_exc(file=sys.stdout)
    items= { "result": "err", "reason": "%s" % (e) }

  response= Response(status=status.HTTP_200_OK, data= items )
  return response


def test_1(request):
  page= "wrapit/test_1.html"
  pageCtxt= { "pageTitle": "St Stephens KMS", "elmCode": "stStephens.js"}
  return page, pageCtxt
