import { useState, useCallback, useMemo, createContext, useContext } from 'react';

const ThemeContext = createContext({ mode: 'dark', toggle: () => {} });

function useDebounce(value, delay) {
  const [debounced, setDebounced] = useState(value);
  useEffect(() => {
    const timer = setTimeout(() => setDebounced(value), delay);
    return () => clearTimeout(timer);
  }, [value, delay]);
  return debounced;
}

const DataGrid = ({ data, columns, onSort, pageSize = 20 }) => {
  const [page, setPage] = useState(0);
  const [sortCol, setSortCol] = useState(null);
  const [sortDir, setSortDir] = useState('asc');
  const { mode } = useContext(ThemeContext);
  
  const sorted = useMemo(() => {
    if (!sortCol) return data;
    return [...data].sort((a, b) => {
      const v = a[sortCol] > b[sortCol] ? 1 : -1;
      return sortDir === 'asc' ? v : -v;
    });
  }, [data, sortCol, sortDir]);
  
  const paged = sorted.slice(page * pageSize, (page + 1) * pageSize);
  const totalPages = Math.ceil(data.length / pageSize);
  
  return (
    <div className={`grid-container ${mode}`}>
      <table>
        <thead>
          <tr>{columns.map(col => (
            <th key={col.key} onClick={() => {
              setSortCol(col.key);
              setSortDir(d => d === 'asc' ? 'desc' : 'asc');
            }}>{col.label} {sortCol === col.key && (sortDir === 'asc' ? '↑' : '↓')}</th>
          ))}</tr>
        </thead>
        <tbody>
          {paged.map((row, i) => (
            <tr key={i}>{columns.map(col => <td key={col.key}>{row[col.key]}</td>)}</tr>
          ))}
        </tbody>
      </table>
      <nav>{Array.from({length: totalPages}, (_, i) => (
        <button key={i} className={i === page ? 'active' : ''} onClick={() => setPage(i)}>{i + 1}</button>
      ))}</nav>
    </div>
  );
};
