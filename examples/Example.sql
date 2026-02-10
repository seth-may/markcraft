-- Advanced SQL: CTEs, Window Functions, Lateral Joins

-- Recursive CTE: Organization hierarchy
WITH RECURSIVE org_tree AS (
    SELECT id, name, manager_id, 1 AS depth,
           ARRAY[name] AS path,
           name::TEXT AS full_path
    FROM employees
    WHERE manager_id IS NULL
    
    UNION ALL
    
    SELECT e.id, e.name, e.manager_id, ot.depth + 1,
           ot.path || e.name,
           ot.full_path || ' > ' || e.name
    FROM employees e
    INNER JOIN org_tree ot ON e.manager_id = ot.id
    WHERE ot.depth < 10
)
SELECT * FROM org_tree ORDER BY path;

-- Window Functions: Running analytics
SELECT
    date,
    revenue,
    SUM(revenue) OVER (ORDER BY date ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS cumulative_revenue,
    AVG(revenue) OVER (ORDER BY date ROWS BETWEEN 6 PRECEDING AND CURRENT ROW) AS moving_avg_7d,
    LAG(revenue, 1) OVER (ORDER BY date) AS prev_day,
    revenue - LAG(revenue, 1) OVER (ORDER BY date) AS daily_change,
    PERCENT_RANK() OVER (ORDER BY revenue) AS percentile,
    NTILE(4) OVER (ORDER BY revenue) AS quartile
FROM daily_metrics
WHERE date >= CURRENT_DATE - INTERVAL '90 days';

-- Lateral Join: Top N per group
SELECT d.name AS department, top_earners.*
FROM departments d
CROSS JOIN LATERAL (
    SELECT e.name, e.salary, e.hire_date
    FROM employees e
    WHERE e.department_id = d.id
    ORDER BY e.salary DESC
    LIMIT 3
) AS top_earners;

-- Materialized View with Refresh
CREATE MATERIALIZED VIEW monthly_summary AS
WITH monthly AS (
    SELECT
        DATE_TRUNC('month', created_at) AS month,
        category,
        COUNT(*) AS total_orders,
        SUM(amount) AS total_revenue,
        AVG(amount) AS avg_order_value,
        COUNT(DISTINCT customer_id) AS unique_customers
    FROM orders
    GROUP BY 1, 2
)
SELECT *,
    total_revenue - LAG(total_revenue) OVER (PARTITION BY category ORDER BY month) AS mom_change,
    ROUND(100.0 * (total_revenue - LAG(total_revenue) OVER (PARTITION BY category ORDER BY month))
        / NULLIF(LAG(total_revenue) OVER (PARTITION BY category ORDER BY month), 0), 2) AS mom_pct
FROM monthly
WITH DATA;
