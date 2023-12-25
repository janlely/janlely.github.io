## 本文介绍业内常见的三种git分支管理策略
Git Flow, Github Flow, Gitlab Flow
## Git Flow
![](../_media/gitflow.png)
###  特点
* 两条主分支: develop, master
* 多条临时分支: feature, release-xxx, hotfix等
### Tips
* feature合并到develop之后同时创建release-xx分支
* 在release-xx分支上进行bug修复，修复完成封版时同时合并到develop和master，在封版前不能合并回develop分支
* release-xx分支用于dev和test环境部署，master分支用于pre,producte分支部署

### 总结
* 综合考虑了feature和hotfix
* 适合一段时间一个版本的项目
* 两条主分支，流程复杂，管理难度大
* develop分支和release-xx分支上都要提交，管理复杂度高


## GitHub Flow
![](../_media/githubflow.png)
### 特点
* 一条主分支master
* 不分区feature与hotfix
### Tips
* 依赖代码审查
### 总结
* 简单
* 合适开源软件
* 合适持续发布
* 对贡献者素质要求高

## Gitlab Flow
[Gitlab Flow介绍](https://docs.gitlab.cn/jh/topics/gitlab_flow.html)
![](../_media/gitflow0.png)
![](../_media/gitlabflow.png)
![](../_media/gitlabflow1.png)
## 特点
* 一条主分支master
* 可以存在多条环境分支
* 可以存在多条发布分支
## Tips
* master为所有环境分支以及发布分支上游，所有的环境分支和发布分支只能merge，不能自己产生commit
* 从master上拉feature分支，开发完成之后合并到master，master合并到test，test合并到product
* hotfix场景，从product拉分支，修复完成合并到product，生成验证没问题之后合并回到master

## 场景与解决方案

### 多feature并行开发
|策略|方案|优缺点|
|--|--|--|
|git flow|- 从develop拉多条feature分支<br> - 每个feature开发完成之后合并到develop并同时创建release分支<br> - release分支上进行测试与bug修复，完成后合并到develop和master|优点:并行开发，也可以并行测试|
|gitlab flow|- 从master拉feature分支<br> - 开发完成之后合并回到master分支，同时删除该feature分支<br> - 前一个feature上线之后打tag，才能将后一个feature合并到master|缺点：feature可以并行开发，但是测试和发布必须串行|


### hotfix
|策略|方案|
|--|--|
|git flow|- 从master拉hotfix分支<br> - 修复问题并合并回master分支进行生产环境验证<br> - 最后合并回develop分支|
|gitlab flow|- 从master分支的指定tag拉新hotfix<br> - 修复完成之后使用hotfix分支在各环境进行测试<br> - 测试完成之后把hotfix合并到master分支和product分支|

## 案例
![](../_media/ecflow.jpg)
![](../_media/someflow.png)