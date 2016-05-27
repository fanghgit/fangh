# coding: UTF-8

from BeautifulSoup import BeautifulSoup
import httplib2
import re

def get_bilibili_zp(html_src):
    'Get the content of bilibili, and produce a list'
    # create soup
    soup = BeautifulSoup(html_src)
    
    # pattern
    zp_html = soup.findAll('ul', id = re.compile("^sp_list$"))
    soup = BeautifulSoup(str(zp_html))
    
    # title
    title_list = soup.findAll('div',{'class':'t'})
    for i, value in enumerate(title_list):
        title_list[i] = str(title_list[i]).replace('\n','')
        title_list[i] = str(title_list[i]).replace('\r','')
        title_list[i] = str(title_list[i]).replace('<em class="keyword">','')
        title_list[i] = str(title_list[i]).replace('</em>','')
    pattern = re.compile("</span>[^<]*" )
    title_list = [ "".join(pattern.findall(each)).replace('</span>','') for each in title_list ]
    
    # image
    img_list = soup.findAll('img')
    for i, value in enumerate(img_list):
        img_list[i] = str(img_list[i]).replace('\n','')
        img_list[i] = str(img_list[i]).replace('\r','')
    pattern = re.compile('<img src="[^"]*' )
    img_list = [ "".join(pattern.findall(each)).replace('<img src="','') for each in img_list ]
    
    # link
    url_list = soup.findAll('a', {'class':'title', 'target':'_blank'})
    for i, value in enumerate(url_list):
        url_list[i] = str(url_list[i]).replace('\n','')
        url_list[i] = str(url_list[i]).replace('\r','')
    pattern = re.compile('href="[^"]*' )
    url_list = [ "".join(pattern.findall(each)).replace('href="','') for each in url_list ]
    url_list = [ "http://www.bilibili.com" + each for each in url_list ]
    
    # click
    click_list = soup('i',{'class':'dj'})
    for i, value in enumerate(click_list):
        click_list[i] = str(click_list[i]).replace('\n','')
        click_list[i] = str(click_list[i]).replace('\r','')
    pattern = re.compile('>[^<]*' )
    click_list = [ "".join(pattern.findall(each)).replace('>','') for each in click_list ]
    
    # content
    content_list = soup.findAll('div', {'class':'intro'})
    for i, value in enumerate(content_list):
        content_list[i] = str(content_list[i]).replace('\n','')
        content_list[i] = str(content_list[i]).replace('\r','')
    pattern = re.compile('<div class="intro">[^<]*' )
    content_list = [ "".join(pattern.findall(each)).replace('<div class="intro">','') for each in content_list ]

    # create list
    dict_key_list = ['title', 'img', 'url', 'click','content']
    dict_value_list = map(None, title_list, img_list, url_list, click_list, content_list)
    obj_list = [ dict( zip(dict_key_list, each_value_list) ) for each_value_list in dict_value_list ]
    return obj_list

def get_bilibili_zt(html_src):
    'get_bilibili_zt'
    # create soup
    soup = BeautifulSoup(html_src)
    
    # pattern
    zp_html = soup.findAll('ul', id = re.compile("^tp_list$"))
    soup = BeautifulSoup(str(zp_html))
    
    # title
    title_list = soup.findAll('div',{'class':'t'})
    for i, value in enumerate(title_list):
        title_list[i] = str(title_list[i]).replace('\n','')
        title_list[i] = str(title_list[i]).replace('\r','')
    pattern = re.compile("</span>[^<]*" )
    title_list = [ "".join(pattern.findall(each)).replace('</span>','') for each in title_list ]
    
    # image
    img_list = soup.findAll('img')
    for i, value in enumerate(img_list):
        img_list[i] = str(img_list[i]).replace('\n','')
        img_list[i] = str(img_list[i]).replace('\r','')
    pattern = re.compile('<img src="[^"]*' )
    img_list = [ "".join(pattern.findall(each)).replace('<img src="','') for each in img_list ]
    
    # link
    url_list = soup.findAll('a', {'class':'title', 'target':'_blank'})
    for i, value in enumerate(url_list):
        url_list[i] = str(url_list[i]).replace('\n','')
        url_list[i] = str(url_list[i]).replace('\r','')
    pattern = re.compile('href="[^"]*' )
    url_list = [ "".join(pattern.findall(each)).replace('href="','') for each in url_list ]
    url_list = [ "http://www.bilibili.com" + each for each in url_list ]
    
    # click
    click_list = soup('i',{'class':'dj'})
    for i, value in enumerate(click_list):
        click_list[i] = str(click_list[i]).replace('\n','')
        click_list[i] = str(click_list[i]).replace('\r','')
    pattern = re.compile('>[^<]*' )
    click_list = [ "".join(pattern.findall(each)).replace('>','') for each in click_list ]
    
    # content
    content_list = soup.findAll('div', {'class':'intro'})
    for i, value in enumerate(content_list):
        content_list[i] = str(content_list[i]).replace('\n','')
        content_list[i] = str(content_list[i]).replace('\r','')
    pattern = re.compile('<div class="intro">[^<]*' )
    content_list = [ "".join(pattern.findall(each)).replace('<div class="intro">','') for each in content_list ]
    
    # create list
    dict_key_list = ['title', 'img', 'url', 'click','content']
    dict_value_list = map(None, title_list, img_list, url_list, click_list, content_list)
    obj_list = [ dict( zip(dict_key_list, each_value_list) ) for each_value_list in dict_value_list ]
    return obj_list    

def get_bilibili_sp(html_src):
    'video content'
    # create soup
    soup = BeautifulSoup(html_src)
    
    # video pattern
    sp_html = soup.findAll('ul', {'class':'result'})
    soup = BeautifulSoup(str(sp_html[-1]))
     
    # title
    title_list = soup.findAll('div',{'class':'t'})
    for i, value in enumerate(title_list):
        title_list[i] = str(title_list[i]).replace('\n','')
    pattern = re.compile("</span>[^<]*" )
    title_list = [ "".join(pattern.findall(each)).replace('</span>','').lstrip().rstrip() for each in title_list ]
    
    # image
    img_list = soup.findAll('img')
    for i, value in enumerate(img_list):
        img_list[i] = str(img_list[i]).replace('\n','')
        img_list[i] = str(img_list[i]).replace('\r','')
    pattern = re.compile('<img src="[^"]*' )
    img_list = [ "".join(pattern.findall(each)).replace('<img src="','') for each in img_list ]
    
    # link
    url_list = soup.findAll('a', {'class':'title', 'target':'_blank'})
    for i, value in enumerate(url_list):
        url_list[i] = str(url_list[i]).replace('\n','')
        url_list[i] = str(url_list[i]).replace('\r','')
    pattern = re.compile('href="[^"]*' )
    url_list = [ "".join(pattern.findall(each)).replace('href="','') for each in url_list ]
    
    # click
    click_list = soup('i',{'class':'gk'})
    for i, value in enumerate(click_list):
        click_list[i] = str(click_list[i]).replace('\n','')
        click_list[i] = str(click_list[i]).replace('\r','')
    pattern = re.compile('>[^<]*' )
    click_list = [ "".join(pattern.findall(each)).replace('>','') for each in click_list ]
    
    # content
    content_list = soup.findAll('div', {'class':'intro'})
    for i, value in enumerate(content_list):
        content_list[i] = str(content_list[i]).replace('\n','')
        content_list[i] = str(content_list[i]).replace('\r','')
    pattern = re.compile('<div class="intro">[^<]*' )
    content_list = [ "".join(pattern.findall(each)).replace('<div class="intro">','') for each in content_list ]

    # create list
    dict_key_list = ['title', 'img', 'url', 'click','content']
    dict_value_list = map(None, title_list, img_list, url_list, click_list, content_list)
    obj_list = [ dict( zip(dict_key_list, each_value_list) ) for each_value_list in dict_value_list ]
    return obj_list

def get_obj_list(bilibili_link):
    http = httplib2.Http() 
    resp, bilibili_src = http.request(bilibili_link, 'GET')
    bilibili_src = bilibili_src.replace('\n','')
    bilibili_src = bilibili_src.replace('\r','')
    obj_list = []
    obj_list += get_bilibili_zp(bilibili_src)
    obj_list += get_bilibili_zt(bilibili_src)
    obj_list += get_bilibili_sp(bilibili_src)
    return obj_list

if __name__ == '__main__':
    pass
