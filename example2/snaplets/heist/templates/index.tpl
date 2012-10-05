<!DOCTYPE html>    
<html>
<head>

<style type="text/css">
    body {
        padding: 20px 30px;
    }
    section {
        margin: 10px 0 0;
        border: 1px solid #CCC;
        padding: 10px 20px;
    }
</style>
</head>

<body>

<section name="testWeibo">

    <h2>1. Login</h2>
    <a href="/weibo">Login with Weibo</a> 

    <h2>2.1 Show User Raw Info</h2>
    <a href="/weibo/account">Weibo Account</a> 
    
    <h2>2.2 Post new and see raw result</h2>
    <form action="/weibo/new" method="POST"> 
        <textarea name="content" row="3" />
        <input type="submit" value="post" name="submit" >
    </form> 
    
</section>

<section name="testTaobao">

    <h2>1. Login</h2>
    <a href="/taobao">Login with Taobao</a> 
    
    <h2>2. Fetch User Info</h2>
    <a href="/taobao/user">User.Get</a> 

    
</section>

</body>
</html>
