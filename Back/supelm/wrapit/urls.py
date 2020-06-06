from django.urls import path
from django.conf.urls import url

from . import views

urlpatterns = [
  path("", views.landingPage, name="index")
  , path("doTest", views.testDispatcher, name="testDispatcer")
  , path("apistub", views.apiStub, name="apiStub")
]
