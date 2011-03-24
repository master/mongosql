%define realname mongosql
Name:		erlang-%{realname}
Version:	0.0.1
Release:	1%{?dist}
Summary:	MongoDB SQL wrapper
Group:		Development/Languages
License:	MIT
URL:		https://github.com/master/mongosql
Source0:	http://cloud.github.com/downloads/master/%{realname}/%{realname}-%{version}.tar.gz
BuildRoot:	%(mktemp -ud %{_tmppath}/%{name}-%{version}-%{release}-XXXXXX)
BuildRequires:	erlang
%if 0%{?el4}%{?el5}%{?fc11}
Requires:	erlang
%else
Requires:	erlang-erts
Requires:	erlang-kernel
Requires:	erlang-stdlib
%endif
Provides:	%{realname} = %{version}-%{release}


%description
MongoDB SQL wrapper


%prep
%setup -q -n %{realname}-%{version}


%build
make %{?_smp_mflags}

%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT%{_libdir}/erlang/lib/%{realname}-%{version}/ebin
for i in ebin/*.beam ebin/*.app; do install $i $RPM_BUILD_ROOT%{_libdir}/erlang/lib/%{realname}-%{version}/$i ; done
rm -f $RPM_BUILD_ROOT%{_libdir}/erlang/lib/%{realname}-%{version}/ebin/mongosql_cli.beam

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%doc COPYING README.md
%dir %{_libdir}/erlang/lib/%{realname}-%{version}
%dir %{_libdir}/erlang/lib/%{realname}-%{version}/ebin
%{_libdir}/erlang/lib/%{realname}-%{version}/ebin/%{realname}.app
%{_libdir}/erlang/lib/%{realname}-%{version}/ebin/mongosql_conn.beam
%{_libdir}/erlang/lib/%{realname}-%{version}/ebin/mongosql_sem.beam
%{_libdir}/erlang/lib/%{realname}-%{version}/ebin/sql92_parser.beam
%{_libdir}/erlang/lib/%{realname}-%{version}/ebin/sql92_scan.beam


%changelog

* Thu Mar 24 2011 Oleg Smirnov <oleg.smirnov@gmail.com> 0.0.1-1
- Initial package
