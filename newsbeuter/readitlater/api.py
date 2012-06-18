#!/usr/bin/env python2.6
# -*- coding: utf-8 -*-

# http://code.google.com/p/soc/wiki/PythonStyleGuide

import logging
import urllib
import urllib2
import json

API_ENDPOINT = 'https://readitlaterlist.com/v2/'

class ReadItLaterError(Exception): pass
class RequestError(ReadItLaterError): pass
class AuthError(ReadItLaterError): pass
class RateLimitExceeded(ReadItLaterError): pass
class ServerError(ReadItLaterError): pass
class APIError(ReadItLaterError): pass

class API(object):
  def __init__(self, api_key, username, password):
    self._status = None
    self.api_key = api_key
    self.username = username
    self.password = password

  def _call(self, method, **params):
    params.update({
      'apikey': self.api_key,
      'username': self.username,
      'password': self.password,
    })
    # opts_dict = vars(opts)
    # Remove parameters that are None. (We assume those to be the defaults.)
    params = dict([(k, v) for k, v in params.items() if v is not None])
    try:
      res = urllib2.urlopen('%s%s?%s' % (
        API_ENDPOINT, method, urllib.urlencode(params)))
      logging.debug(res.url)
      self._status = {
        'User-Limit': res.headers.get('X-Limit-User-Limit'),
        'User-Remaining': res.headers.get('X-Limit-User-Remaining'),
        'User-Reset': res.headers.get('X-Limit-User-Reset'),
        'Key-Limit': res.headers.get('X-Limit-Key-Limit'),
        'Key-Remaining': res.headers.get('X-Limit-Key-Remaining'),
        'Key-Reset': res.headers.get('X-Limit-Key-Reset'), }
      logging.debug(self._status)
      return res.read()
    except urllib2.URLError, e:
      logging.debug(e.url)
      logging.error(e)

      if e.code == 400:
        raise RequestError('Invalid request, please make sure '
          'you follow the documentation for proper syntax')
      elif e.code == 401:
        raise AuthError('Username and/or password is incorrect')
      elif e.code == 403:
        raise RateLimitExceeded('Rate limit exceeded, please '
          'wait a little bit before resubmitting')
      elif e.code == 503:
        raise ServerError('Read It Later\'s sync server is down '
          'for scheduled maintenance')
      else:
        raise ReadItLaterError('Unknown error occurred while '
          'requesting readitlater')

  def auth(self):
    self._call('auth')
    return True

  def add(self, url, title=None):
    self._call('add', url=url, title=title)
    return True

  def send(self, **params):
    '''
    Send changes using the `send` method

    See more about this API here:
    http://readitlaterlist.com/api/docs/#send

    Examples
    --------

    >>> new = [
    ...   {"url":"http://google.com",
    ...    "title":"Google"
    ...    "ref_id":"123456709"
    ...    },
    ...    {"url":"http://ideashower.com"
    ...     "title":"The Idea Shower"
    ...    },
    ... ]
    >>> api.send(new=new)

    '''
    jsonparams = dict(
      (key, json.dumps(
        dict((str(i), val) for (i, val) in enumerate(vallist))))
      for (key, vallist) in params.iteritems())
    self._call('send', **jsonparams)
    return True

  def get(self, state=None, my_app_only=False, since=None, count=None,
       page=None, tags=False):
    '''Retrieve List.
    :param state: Get only read or unread items. Valid values: "read" or
      "unread".If not provided, defaults to all.
    :type state: string or None
    :param my_app_only: Only retrieve pages saved from your api key.
    :type my_app_only: boolean or None
    :param since: Only get changed/added items since this time.
    :type since: datetime.datetime or None
    :param count: Number of items to retrieve.
    :type count: integer or None
    :param page: Used with count to paginate results.
    :type page: integer or None
    :param tags: Retrieve tags with items.
    :type tags: boolean or None
    :rtype: dict with status, since and list keys.
    '''
    params = {}
    if state is not None:
      if state not in ('read', 'unread'):
        raise APIError('Invalid parameter: state')
      params['state'] = state
    if my_app_only:
      params['myAppOnly'] = 1
    if since is not None:
      params['since'] = mktime(since.timetuple())
    params['count'] = count
    params['page'] = page
    if tags:
      params['tags'] = 1

    json_doc = self._call('get', **params)
    return json.loads(json_doc)

  def status(self):
    if not self._status:
      self._call('api')
    return self._status

if __name__ == '__main__':
  logging.basicConfig(level=logging.DEBUG)
