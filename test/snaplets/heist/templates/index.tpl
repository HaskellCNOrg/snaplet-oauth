<!DOCTYPE html>    
<html>
<head></head>

<body>

<h2>1. Login</h2>
<p> 
    <a href="/weibo">Login with Weibo</a> 
</p> 

<h2>2. Post new and see raw result</h2>
    <form action="/postnew" method="POST"> 
        <label for="content">New content</label> 
        <!-- <input type="textarea" name="content" > -->
        <textarea name="content" row="3" />
        <input type="submit" value="post" name="submit" >
    </form> 
    
</body>
</html>