create table account
 (account_id integer primary key autoincrement not null,
  product_cd varchar(10) not null,
  cust_id integer not null,
  open_date date not null,
  close_date date,
  last_activity_date date,
  status text not null,
  open_branch_id integer,
  open_emp_id integer,
  avail_balance float(10,2),
  pending_balance float(10,2),
  check(status = 'ACTIVE' or status = 'CLOSED' or status = 'FROZEN')
  constraint fk_product_cd foreign key (product_cd)
    references product (product_cd),
  constraint fk_a_cust_id foreign key (cust_id)
    references customer (cust_id),
  constraint fk_a_branch_id foreign key (open_branch_id)
    references branch (branch_id),
  constraint fk_a_emp_id foreign key (open_emp_id)
    references employee (emp_id)
 );
