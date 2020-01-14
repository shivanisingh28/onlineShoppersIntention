# onlineShoppersIntention

Data Source
In Big data, data source mainly has been categorized as social data, machine data or transactional data.
The type of data being used in the project falls under social data because the data is been recorded by
social interaction with customers via the Internet. However, there are many other categories for data
source such as unstructured that means the data is not recognised and used for interpretation.
The source has been taken by the Kaggle and posted by Roshan Sharma from the below mentioned
authors:
1. C. Okan Sakar Department of Computer Engineering, Faculty of Engineering and Natural Sciences,
Bahcesehir University, 34349 Besiktas, Istanbul, Turkey
2. Yomi Kastro Inveon Information Technologies Consultancy and Trade, 34335 Istanbul, Turkey
Data Description
We have about 12331 instances and 18 features for an Online shopping dataset.
The features can be described are as below:
1. Administrative -> Administrative Value
2. Administrative_Duration -> Duration in Administrative Page
3. Informational -> Informational Value
4. Informational_Duration -> Duration in Informational Page
5. ProductRelated -> Product Related Value
6. ProductRelated_Duration -> Duration in Product Related Page
7. BounceRates -> Bounce Rates of a web page
8. ExitRates -> Exit rate of a web page
9. PageValues -> Page values of each web page
10. SpecialDay -> Special days like valentine etc
11. Month -> Month of the year
12. OperatingSystems -> Operating system used
13. Browser -> Browser used
14. Region -> Region of the user
15. TrafficType -> Traffic Type
16. VisitorType -> Types of Visitor
17. Weekend -> Weekend or not
18. Revenue -> Revenue will be generated or not
"Administrative", "Administrative Duration", "Informational", "Informational Duration", "Product
Related" and "Product Related Duration" represent the number of different types of pages visited by the
visitor in that session and total time spent in each of these page categories. The values of these features
are derived from the URL information of the pages visited by the user and updated in real time when a
user takes an action, e.g. moving from one page to another. The "Bounce Rate", "Exit Rate" and "Page
Value" features represent the metrics measured by "Google Analytics" for each page in the e-commerce
site. The value of "Bounce Rate" feature for a web page refers to the percentage of visitors who enter
the site from that page and then leave ("bounce") without triggering any other requests to the analytics
server during that session. The value of "Exit Rate" feature for a specific web page is calculated as for all
pageviews to the page, the percentage that were the last in the session. The "Page Value" feature
represents the average value for a web page that a user visited before completing an e-commerce
transaction. The "Special Day" feature indicates the closeness of the site visiting time to a specific special
day (e.g. Mother’s Day, Valentine's Day) in which the sessions are more likely to be finalized with
transaction. The value of this attribute is determined by considering the dynamics of e-commerce such
as the duration between the order date and delivery date. For example, for Valentina’s day, this value
takes a nonzero value between February 2 and February 12, zero before and after this date unless it is
close to another special day, and its maximum value of 1 on February 8. The dataset also includes
operating system, browser, region, traffic type, visitor type as returning or new visitor, a Boolean value
indicating whether the date of the visit is weekend, and month of the year.
