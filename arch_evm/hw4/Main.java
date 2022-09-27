import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Scanner;

class Comm16 implements CommInter {
    public String opc;
    public String nz1;
    public String nz2;
    public String rs;
    public String f3;
    public String full;

    public Comm16(String opc, String nz1, String rs, String nz2, String f3, String full) {
        this.opc = opc;
        this.nz1 = nz1;
        this.nz2 = nz2;
        this.rs = rs;
        this.f3 = f3;
        this.full = full;
    }

    public String returnBits(int start, int end) {
        return new StringBuilder(full.substring(start, end + 1)).reverse().toString();
    }

    @Override
    public String toString() {
        return new StringBuilder(full).reverse().toString();
    }
}

class Comm32 implements CommInter {
    public String opc;
    public int rd;
    public String f3;
    public int rs1;
    public String imm;
    public String full;

    public Comm32(String opcode, String rd, String f3, String rs1, String imm, String full) {
        this.opc = opcode;
        this.rd = Integer.parseInt(rd, 2);
        this.f3 = f3;
        this.rs1 = Integer.parseInt(rs1, 2);
        this.imm = imm;
        this.full = full;
    }

    public String returnBits(int start, int end) {
        return new StringBuilder(full.substring(start, end + 1)).reverse().toString();
    }

    @Override
    public String toString() {
        return new StringBuilder(full).reverse().toString();
    }
}

interface CommInter {
}

class PastComm implements CommInter {
    public long offset;
    public long num;
    public String command;

    public PastComm(long num, String command, long offset) {
        this.command = command;
        this.num = num;
        this.offset = offset;
    }
}

class Const {

    public String getBind(int x) {
        return switch (x) {
            case 0 -> "LOCAL";
            case 1 -> "GLOBAL";
            case 2 -> "WEAK";
            case 10 -> "LOOS";
            case 12 -> "HIOS";
            case 13 -> "LOPROC";
            case 15 -> "HIPROC";
            default -> null;
        };
    }

    public String getTypes(int x) {
        return switch (x) {
            case 0 -> "NOTYPE";
            case 1 -> "OBJECT";
            case 2 -> "FUNC";
            case 3 -> "SECTION";
            case 4 -> "FILE";
            case 5 -> "COMMON";
            case 6 -> "TLS";
            case 10 -> "LOOS";
            case 12 -> "HIOS";
            case 13 -> "LOPROC";
            case 15 -> "HIPROC";
            default -> null;
        };
    }

    public String getVises(int x) {
        return switch (x) {
            case 0 -> "DEFAULT";
            case 1 -> "INTERNAL";
            case 2 -> "HIDDEN";
            case 3 -> "PROTECTED";
            default -> null;
        };
    }

    public String getSpecial(int x) {
        return switch (x) {
            case 0 -> "UNDEF";
            case 65820 -> "LOPROC";
            case 65311 -> "HIPROC";
            case 65312 -> "LOOS";
            case 65343 -> "HIOS";
            case 65521 -> "ABS";
            case 65522 -> "COMMON";
            case 65535 -> "HIRESERVE";
            default -> null;
        };
    }

    public String getReg(int x) {
        return switch (x) {
            case 0 -> "zero";
            case 1 -> "ra";
            case 2 -> "sp";
            case 3 -> "gp";
            case 4 -> "tp";
            case 5 -> "t0";
            case 6 -> "t1";
            case 7 -> "t2";
            case 8 -> "s0";
            case 9 -> "s1";
            case 10 -> "a0";
            case 11 -> "a1";
            case 12 -> "a2";
            case 13 -> "a3";
            case 14 -> "a4";
            case 15 -> "a5";
            case 16 -> "a6";
            case 17 -> "a7";
            case 18 -> "s2";
            case 19 -> "s3";
            case 20 -> "s4";
            case 21 -> "s5";
            case 22 -> "s6";
            case 23 -> "s7";
            case 24 -> "s8";
            case 25 -> "s9";
            case 26 -> "s10";
            case 27 -> "s11";
            case 28 -> "t3";
            case 29 -> "t4";
            case 30 -> "t5";
            case 31 -> "t6";
            default -> null;
        };
    }

    public String getRegRvc(int x) {
        return switch (x) {
            case 0 -> "s0";
            case 1 -> "s1";
            case 2 -> "a0";
            case 3 -> "a1";
            case 4 -> "a2";
            case 5 -> "a3";
            case 6 -> "a4";
            case 7 -> "a5";
            default -> null;
        };
    }

    public String getRegCsr(int x) {
        return switch (x) {
            case 0x000 -> "ustatus";
            case 0x004 -> "uie";
            case 0x005 -> "utvec";
            case 0x040 -> "uscratch";
            case 0x041 -> "uepc";
            case 0x042 -> "ucause";
            case 0x043 -> "ubadaddr";
            case 0x044 -> "uip";
            case 0x001 -> "fflags";
            case 0x002 -> "frm";
            case 0x003 -> "fcsr";
            case 0xC00 -> "cycle";
            case 0xC01 -> "time";
            case 0xC02 -> "instret";
            case 0xC03 -> "hpmcounter3";
            case 0xC04 -> "hpmcounter4";
            case 0xC1F -> "hpmcounter31";
            case 0xC80 -> "cycleh";
            case 0xC81 -> "timeh";
            case 0xC82 -> "instreth";
            case 0xC83 -> "hpmcounter3h";
            case 0xC84 -> "hpmcounter4h";
            case 0xC9F -> "hpmcounter31h";
            case 0x100 -> "sstatus";
            case 0x102 -> "sedeleg";
            case 0x103 -> "sideleg";
            case 0x104 -> "sie";
            case 0x105 -> "stvec";
            case 0x140 -> "sscratch";
            case 0x141 -> "sepc";
            case 0x142 -> "scause";
            case 0x143 -> "sbadaddr";
            case 0x144 -> "sip";
            case 0x180 -> "sptbr";
            case 0x200 -> "hstatus";
            case 0x202 -> "hedeleg";
            case 0x203 -> "hideleg";
            case 0x204 -> "hie";
            case 0x205 -> "htvec";
            case 0x240 -> "hscratch";
            case 0x241 -> "hepc";
            case 0x242 -> "hcause";
            case 0x243 -> "hbadaddr";
            case 0x244 -> "hip";
            case 0xF11 -> "mvendorid";
            case 0xF12 -> "marchid";
            case 0xF13 -> "mimpid";
            case 0xF14 -> "mhartid";
            case 0x300 -> "mstatus";
            case 0x301 -> "misa";
            case 0x302 -> "medeleg";
            case 0x303 -> "mideleg";
            case 0x304 -> "mie";
            case 0x305 -> "mtvec";
            case 0x340 -> "mscratch";
            case 0x341 -> "mepc";
            case 0x342 -> "mcause";
            case 0x343 -> "mbadaddr";
            case 0x344 -> "mip";
            case 0x380 -> "mbase";
            case 0x381 -> "mbound";
            case 0x382 -> "mibase";
            case 0x383 -> "mibound";
            case 0x384 -> "mdbase";
            case 0x385 -> "mdbound";
            case 0xB00 -> "mcycle";
            case 0xB02 -> "minstret";
            case 0xB03 -> "mhpmcounter3";
            case 0xB04 -> "mhpmcounter4";
            case 0xB1F -> "mhpmcounter31";
            case 0xB80 -> "mcycleh";
            case 0xB82 -> "minstreth";
            case 0xB83 -> "mhpmcounter3h";
            case 0xB84 -> "mhpmcounter4h";
            case 0xB9F -> "mhpmcounter31h";
            case 0x320 -> "mucounteren";
            case 0x321 -> "mscounteren";
            case 0x322 -> "mhcounteren";
            case 0x323 -> "mhpmevent3";
            case 0x324 -> "mhpmevent4";
            case 0x33F -> "mhpmevent31";
            case 0x7A0 -> "tselect";
            case 0x7A1 -> "tdata1";
            case 0x7A2 -> "tdata2";
            case 0x7A3 -> "tdata3";
            case 0x7B0 -> "dcsr";
            case 0x7B1 -> "dpc";
            case 0x7B2 -> "dscratch";
            default -> null;
        };
    }
}

class DizAsmBuilder {
    int[] bytes;

    public DizAsmBuilder(int[] bytes) {
        this.bytes = bytes;
    }

    public long[] returnParam(int start, int[] sz) {
        long[] s = new long[sz.length];
        for (int i = 0; i < sz.length; i++) {
            s[i] = returnBytes(start, sz[i]);
            start += sz[i];
        }
        return s;
    }

    public Head returnHeader() {
        int[] sz = new int[]{16, 2, 2, 4, 4, 4, 4, 4, 2, 2, 2, 2, 2, 2};
        long[] szNew = returnParam(0, sz);
        return new Head(szNew[12], szNew[6], szNew[13]);
    }

    public Section returnSect(long start) {
        int[] base = new int[]{4, 4, 4, 4, 4, 4, 4, 4, 4, 4};
        long[] base1 = returnParam((int) start, base);
        return new Section(base1[0], base1[1], base1[2], base1[3], base1[4], base1[5], base1[6], base1[7], base1[8], base1[9]);
    }

    public long returnBytes(int start, int length) {
        long res = 0;
        for (int i = start + length - 1; i > start - 1; i--) {
            res <<= 8;
            res += bytes[i];
        }
        return res;
    }

    public Entr returnEntry(int start, int symtab_entry_offset) {
        int[] sz = new int[]{4, 4, 4, 1, 1, 2};
        long[] base1 = returnParam(start, sz);
        return new Entr(base1[0], base1[1], base1[2], base1[3], base1[4], base1[5], symtab_entry_offset, this);
    }

    public String returnSectName(Section sect, int offset) {
        return returnStartName(sect.sName + offset);
    }

    public String returnStartName(long start) {
        StringBuilder nm = new StringBuilder();
        long i = start;
        while (i < bytes.length) {
            if (bytes[(int) i] == 0) {
                return nm.toString();
            } else nm.append((char) bytes[(int) i]);
            i++;
        }
        return nm.toString();
    }

    public CommInter returnComm(long number, int cmdLength) {
        StringBuilder bits = new StringBuilder();
        while (number > 0) {
            bits.append(number % 2);
            number /= 2;
        }
        if (cmdLength == 32) {
            int lng = bits.length();
            bits.append("0".repeat(Math.max(0, 32 - lng)));
            String opcode = new StringBuilder(bits.substring(0, 7)).reverse().toString();
            String rd = new StringBuilder(bits.substring(7, 12)).reverse().toString();
            String funct3 = new StringBuilder(bits.substring(12, 15)).reverse().toString();
            String rs1 = new StringBuilder(bits.substring(15, 20)).reverse().toString();
            String imm = new StringBuilder(bits.substring(20, 32)).reverse().toString();
            String full = bits.substring(0, 32);
            return new Comm32(opcode, rd, funct3, rs1, imm, full);
        } else {
            int lng = bits.length();
            bits.append("0".repeat(Math.max(0, 16 - lng)));
            String opcode = new StringBuilder(bits.substring(0, 2)).reverse().toString();
            String nzimm_1 = new StringBuilder(bits.substring(2, 7)).reverse().toString();
            String rs = new StringBuilder(bits.substring(7, 12)).reverse().toString();
            String nzimm_2 = new StringBuilder(bits.substring(12, 13)).reverse().toString();
            String funct3 = new StringBuilder(bits.substring(13, 16)).reverse().toString();
            String full = bits.substring(0, 16);
            return new Comm16(opcode, nzimm_1, rs, nzimm_2, funct3, full);
        }
    }
}

class Entr {
    public long stnm;
    public long stv;
    public long stsz;
    public long stinf;
    public long stoth;
    public long stshndx;
    public String bind;
    public String type;
    public String vis;
    public String index;
    public String name;

    public Entr(long stnm, long stv, long stsz, long stinf, long stoth, long stshndx, long smtbNameOff, DizAsmBuilder builder) {
        Const c = new Const();
        this.stnm = stnm;
        this.stv = stv;
        this.stsz = stsz;
        this.stinf = stinf;
        this.stoth = stoth;
        this.stshndx = stshndx;
        this.bind = c.getBind((int) (stinf >> 4));
        this.type = c.getTypes((int) (stinf & 0xf));
        this.vis = c.getVises((int) (stoth & 0x3));
        if (c.getSpecial((int) this.stshndx) != null) {
            this.index = c.getSpecial((int) this.stshndx);
        } else this.index = Long.toString(this.stshndx);
        this.name = builder.returnStartName(this.stnm + smtbNameOff);
    }

    public String just(String body, char flag, int length, char fuller) {
        StringBuilder str = new StringBuilder(body);
        if (flag == 'l') {
            while (str.length() < length) {
                str.append(fuller);
            }
            return str.toString();
        } else {
            while (str.length() < length) {
                str.insert(0, fuller);
            }
            return str.toString();
        }
    }

    public String get_res(int num) {
        return "[" + just(Integer.toString(num), 'r', 4, ' ') + "] " +
                "0x" + just(Long.toHexString(stv), 'l', 15, ' ') + " " +
                just(Long.toString(stsz), 'r', 5, ' ') + " " +
                just(type, 'l', 8, ' ') + " " +
                just(bind, 'l', 8, ' ') + " " +
                just(vis, 'l', 8, ' ') + " " +
                just(index, 'r', 6, ' ') + " " + name;
    }
}

class Head {
    public long eshff;
    public long eshnm;
    public long eshst;

    public Head(long e_eshnum, long eshff, long eshst) {
        this.eshnm = e_eshnum;
        this.eshff = eshff;
        this.eshst = eshst;
    }
}

public class Main {

    public static String makeString(String body, char flag, int length, char fuller) {
        StringBuilder str = new StringBuilder(body);
        if (flag == 'l') {
            while (str.length() < length) {
                str.append(fuller);
            }
            return str.toString();
        } else {
            while (str.length() < length) {
                str.insert(0, fuller);
            }
            return str.toString();
        }
    }

    public static void main(String[] args) throws IOException {
        try {
            DataInputStream in = new DataInputStream(new FileInputStream(args[0]));
            FileWriter out = new FileWriter(args[1], StandardCharsets.UTF_8);
            byte[] bytes1 = in.readAllBytes();
            int[] bytes = new int[bytes1.length];
            for (int i = 0; i < bytes1.length; i++) {
                bytes[i] = ((bytes1[i] < 0) ? 256 : 0) + bytes1[i];
            }
            DizAsmBuilder builder = new DizAsmBuilder(bytes);
            Head hd = builder.returnHeader();
            long numOfSect = hd.eshnm;
            ArrayList<Section> sections = new ArrayList<>();
            ArrayList<Entr> symEntr = new ArrayList<>();
            ArrayList<PastComm> buildedCd = new ArrayList<>();
            HashMap<Long, String> lDict = new HashMap<>();
            HashMap<Long, String> rDict = new HashMap<>();
            for (int i = 0; i < numOfSect; i++) {
                sections.add(builder.returnSect(hd.eshff + i * 40L));
            }
            int symOffset = 0;
            int numberSymEntries = 0;
            int symNameOffset = 0;
            int cdOffset = 0;
            int cdSize = 0;
            int cdAddress = 0;
            int loCounter = 0;
            Section name_table_section = sections.get((int) hd.eshst);
            for (Section sect : sections) {
                String name = builder.returnSectName(sect, (int) name_table_section.sOff);
                switch (name) {
                    case ".symtab" -> {
                        symOffset = (int) sect.sOff;
                        numberSymEntries = (int) (sect.sSize / 16);
                    }
                    case ".strtab" -> symNameOffset = (int) sect.sOff;
                    case ".text" -> {
                        cdOffset = (int) sect.sOff;
                        cdSize = (int) sect.sSize;
                        cdAddress = (int) sect.sAddr;
                    }
                }
            }
            for (int i = 0; i < numberSymEntries; i++) {
                symEntr.add(builder.returnEntry(symOffset + i * 16, symNameOffset));
                if (!symEntr.get(i).toString().equals("")) {
                    lDict.put(symEntr.get(i).stv, symEntr.get(i).name);
                }
            }
            int i = cdOffset;
            int end = cdOffset + cdSize;
            out.write(".text" + "\n");
            int cnt = 0;
            long skipper;
            boolean offsetFlag;
            Translator x = new Translator();
            while (i < end) {
                long offset = -1;
                long num = cnt + cdAddress;
                AnswStructRemade resi = x.parse_command(i, builder);
                String res = resi.body.result;
                offsetFlag = resi.body.flag;
                skipper = resi.body.number;
                int bits = resi.bit;
                if (offsetFlag && lDict.containsKey(num + skipper)) {
                    rDict.put(num, lDict.get(num + skipper));
                }
                if (offsetFlag) {
                    offset = skipper;
                }
                buildedCd.add(new PastComm(num, res, offset));
                i += bits;
                cnt += bits;
            }
            for (PastComm code : buildedCd) {
                long offset = code.offset;
                long num = code.num;
                if (offset != -1) {
                    long new_num = num + offset;
                    if ((lDict.containsKey(new_num))) {
                        rDict.put(num, lDict.get(new_num));
                    } else if ((!lDict.containsKey(num)) && rDict.containsKey(num)) {
                        lDict.put(new_num, rDict.get(num));
                    } else {
                        lDict.put(new_num, "LOC_" + makeString(Integer.toHexString(loCounter), 'r', 5, '0'));
                        loCounter++;
                    }
                }
            }
            for (PastComm code : buildedCd) {
                long num = code.num;
                String res = code.command;
                Scanner sc = new Scanner(res);
                String com = sc.next();
                com = com.toUpperCase();
                res = res.substring(com.length());
                res = com + res;
                String rLbl = "";
                String lLbl = "";
                if (lDict.containsKey(code.num)) {
                    lLbl = lDict.get(num);
                }
                if (rDict.containsKey(code.num) && rDict.get(code.num).charAt(0) != 'L') {
                    rLbl = rDict.get(num);
                }
                if (lLbl.length() > 0) {
                    lLbl += ":";
                }
                if (rLbl.length() > 0) {
                    rLbl = ", " + rLbl;
                }
                out.write(makeString(Long.toHexString(num), 'r', 8, '0') + " " + makeString(lLbl, 'r', 11, ' ') + " " + res + rLbl + "\n");
            }
            out.write("\n");
            out.write(".symtab" + "\n");
            out.write("Symbol Value              Size Type     Bind     Vis       Index Name" + "\n");
            for (int k = 0; k < symEntr.size(); k++) {
                out.write(symEntr.get(k).get_res(k) + "\n");
            }
            out.close();
            in.close();
        } catch (FileNotFoundException e) {
            System.out.println("File not found");
        } catch (ArrayIndexOutOfBoundsException e) {
            System.out.println("No cmd arguments");
        }
    }
}

class Translator {
    Const a = new Const();

    public AnswStruct output_command16(Comm16 cmd) {
        String f3 = cmd.f3;
        String opcode = cmd.opc;
        String nm = "";
        switch (opcode) {
            case "00" -> {
                String rs2 = a.getRegRvc(Integer.parseInt(cmd.returnBits(2, 4), 2));
                String rs1 = a.getRegRvc(Integer.parseInt(cmd.returnBits(7, 9), 2));
                int uimm = Integer.parseInt(cmd.returnBits(5, 5) + cmd.returnBits(10, 12) + cmd.returnBits(6, 6), 2) * 4;
                switch (f3) {
                    case "000" -> {
                        int imm = Integer.parseInt(cmd.returnBits(7, 10) + cmd.returnBits(11, 12) + cmd.returnBits(5, 5) + cmd.returnBits(6, 6), 2) * 4;
                        if (imm == 0) return new AnswStruct("unknown", false, -1);
                        nm = "c.addi4spn";
                        String rd = a.getRegRvc(Integer.parseInt(cmd.returnBits(2, 4), 2));
                        return new AnswStruct(nm + " " + rd + ", sp, " + imm, false, -1);
                    }
                    case "010" -> nm = "c.lw";
                    case "110" -> nm = "c.sw";
                    default -> {
                        return new AnswStruct("unknown", false, -1);
                    }
                }
                return new AnswStruct(nm + " " + rs2 + ", " + uimm + "(" + rs1 + ")", false, -1);
            }
            case "01" -> {
                switch (f3) {
                    case "000": {
                        if (Integer.parseInt(cmd.returnBits(7, 11), 2) == 0) {
                            nm = "c.nop";
                            return new AnswStruct(nm, false, -1);
                        } else {
                            nm = "c.addi";
                            String rs1 = a.getReg(Integer.parseInt(cmd.returnBits(7, 11), 2));
                            int imm = Integer.parseInt(cmd.returnBits(2, 6), 2) - Integer.parseInt(cmd.returnBits(12, 12)) * 32;
                            return new AnswStruct(nm + " " + rs1 + ", " + imm, false, -1);
                        }
                    }
                    case "001": {
                        nm = "c.jal";
                        int offset = Integer.parseInt(cmd.returnBits(8, 8) + cmd.returnBits(10, 10) +
                                cmd.returnBits(9, 9) + cmd.returnBits(6, 6) + cmd.returnBits(7, 7) + cmd.returnBits(2, 2) + cmd.returnBits(11, 11) + cmd.returnBits(3, 5), 2) * 2 - Integer.parseInt(cmd.returnBits(12, 12)) * 2048;
                        return new AnswStruct(nm, true, offset);
                    }
                    case "010": {
                        nm = "c.li";
                        String rd = a.getReg(Integer.parseInt(cmd.returnBits(7, 11), 2));
                        int imm = Integer.parseInt(cmd.returnBits(2, 6), 2) - 32 * Integer.parseInt(cmd.returnBits(12, 12));
                        return new AnswStruct(nm + " " + rd + ", " + imm, false, -1);
                    }
                    case "011": {
                        switch (Integer.parseInt(cmd.returnBits(7, 11), 2)) {
                            case 2 -> {
                                nm = "c.addi16sp";
                                int nzimm = Integer.parseInt(cmd.returnBits(3, 4) + cmd.returnBits(5, 5) + cmd.returnBits(2, 2) + cmd.returnBits(6, 6), 2) * 16 - 512 * Integer.parseInt(cmd.returnBits(12, 12));
                                return new AnswStruct(nm + " sp, " + nzimm, false, -1);
                            }
                            case 0 -> {
                                return new AnswStruct("unknown", false, -1);
                            }
                            default -> {
                                int nzimm = Integer.parseInt(cmd.returnBits(12, 12) + cmd.returnBits(2, 6), 2) * 4096;
                                String rd = a.getReg(Integer.parseInt(cmd.returnBits(7, 11), 2));
                                return new AnswStruct("c.lui " + rd + ", " + nzimm, false, -1);
                            }
                        }
                    }
                    case "100": {
                        int nzimm = Integer.parseInt(cmd.returnBits(12, 12) + cmd.returnBits(2, 6), 2);
                        String rd = a.getRegRvc(Integer.parseInt(cmd.returnBits(7, 9), 2));
                        int imm = Integer.parseInt(cmd.returnBits(2, 6), 2) - Integer.parseInt(cmd.returnBits(12, 12)) * 32;
                        switch (cmd.returnBits(10, 11)) {
                            case "00" -> {
                                return new AnswStruct("c.srli " + rd + ", " + nzimm, false, -1);
                            }
                            case "01" -> {
                                return new AnswStruct("c.rai " + rd + ", " + nzimm, false, -1);
                            }
                            case "10" -> {
                                return new AnswStruct("c.andi " + rd + ", " + imm, false, -1);
                            }
                            case "11" -> {
                                switch (cmd.returnBits(5, 6)) {
                                    case "00" -> nm = "c.sub";
                                    case "01" -> nm = "c.xor";
                                    case "10" -> nm = "c.or";
                                    case "11" -> nm = "c.and";
                                }
                                String rs2 = a.getRegRvc(Integer.parseInt(cmd.returnBits(2, 4), 2));
                                String rs1 = a.getRegRvc(Integer.parseInt(cmd.returnBits(7, 9), 2));
                                return new AnswStruct(nm + " " + rs1 + ", " + rs2, false, -1);

                            }
                        }
                    }
                    case "101": {
                        nm = "c.j";
                        int offset = Integer.parseInt(cmd.returnBits(8, 8) + cmd.returnBits(10, 10) + cmd.returnBits(9, 9) + cmd.returnBits(6, 6) + cmd.returnBits(7, 7) +
                                cmd.returnBits(2, 2) + cmd.returnBits(11, 11) + cmd.returnBits(3, 5), 2) * 2 - Integer.parseInt(cmd.returnBits(12, 12)) * 2048;
                        return new AnswStruct(nm, true, offset);
                    }
                    case "111": {
                        int offset = Integer.parseInt(cmd.returnBits(12, 12) + cmd.returnBits(5, 6) + cmd.returnBits(2, 2) + cmd.returnBits(10, 11) + cmd.returnBits(3, 4), 2) * 2 - 2 * 256 * Integer.parseInt(cmd.returnBits(12, 12));
                        String rs1 = a.getRegRvc(Integer.parseInt(cmd.returnBits(7, 9), 2));
                        return new AnswStruct("c.bnez " + rs1, true, offset);
                    }
                    default: {
                        return new AnswStruct("unknown", false, -1);
                    }
                }
            }
            case "10" -> {
                switch (f3) {
                    case "000" -> {
                        String rs1 = a.getReg(Integer.parseInt(cmd.returnBits(7, 11), 2));
                        int nzuimm = Integer.parseInt(cmd.returnBits(12, 12) + cmd.returnBits(2, 6), 2);
                        return new AnswStruct("c.slli " + rs1 + ", " + nzuimm, false, -1);
                    }
                    case "010" -> {
                        String rd = a.getReg(Integer.parseInt(cmd.returnBits(7, 11), 2));
                        int uimm = Integer.parseInt(cmd.returnBits(2, 3) + cmd.returnBits(12,
                                12) + cmd.returnBits(4, 6), 2) * 4;
                        return new AnswStruct("c.lwsp " + rd + ", " + uimm + "(sp)", false, -1);
                    }
                    case "100" -> {
                        if (Integer.parseInt(cmd.returnBits(2, 6), 2) == 0) {
                            if (Integer.parseInt(cmd.returnBits(7, 11)) == 0) {
                                return new AnswStruct("c.ebreak", false, -1);
                            }
                            String rs1 = a.getReg(Integer.parseInt(cmd.returnBits(7, 11), 2));
                            if (cmd.returnBits(12, 12).equals("0")) {
                                return new AnswStruct("c.jr " + rs1, false, -1);
                            } else {
                                return new AnswStruct("c.jalr " + rs1, false, -1);
                            }
                        }
                        String rd = a.getReg(Integer.parseInt(cmd.returnBits(7, 11), 2));
                        String rs2 = a.getReg(Integer.parseInt(cmd.returnBits(2, 6), 2));
                        if (cmd.returnBits(12, 12).equals("0")) {
                            nm = "c.mv";
                        } else nm = "c.add";
                        return new AnswStruct(nm + " " + rd + ", " + rs2, false, -1);
                    }
                    case "110" -> {
                        nm = "c.swsp";
                        int uimm = Integer.parseInt(cmd.returnBits(7, 8) + cmd.returnBits(9, 12), 2) * 4;
                        String rs2 = a.getReg(Integer.parseInt(cmd.returnBits(2, 6), 2));
                        return new AnswStruct(nm + " " + rs2 + ", " + uimm + "(sp)", false, -1);
                    }
                    default -> {
                        return new AnswStruct("unknown", false, -1);
                    }
                }
            }
            default -> {
                return new AnswStruct("unknown", false, -1);
            }
        }
    }

    public AnswStruct output_command32(Comm32 cmd) {
        int rd = cmd.rd;
        int imm = Integer.parseInt(cmd.imm, 2) - 2 * 2048 * Integer.parseInt(cmd.returnBits(31, 31));
        int rs1 = cmd.rs1;
        switch (cmd.opc) {
            case "0110111" -> {
                int temp_imm = Integer.parseInt(cmd.returnBits(12, 31), 2) * 4096;
                return new AnswStruct("lui " + a.getReg(rd) + ", " + temp_imm, false, -1);
            }
            case "0010111" -> {
                long temp_imm = (long) (Integer.parseInt(cmd.returnBits(12, 30), 2) * 4096L -
                        Integer.parseInt(cmd.returnBits(31, 31)) * Math.pow(2, 31));
                return new AnswStruct("auipc " + a.getReg(rd) + ", " + temp_imm, false, -1);
            }
            case "1101111" -> {
                long temp_imm = Long.parseLong(cmd.returnBits(31, 31) + cmd.returnBits(12, 19) + cmd.returnBits(20, 20) + cmd.returnBits(21, 30), 2) * 2 - (1 << 21) * Long.parseLong(cmd.returnBits(31, 31));
                return new AnswStruct("jal " + a.getReg(rd), true, temp_imm);
            }
            case "1100111" -> {
                return new AnswStruct("jalr " + a.getReg(rd) + ", " + imm + "(" + a.getReg(rs1) + ")", false, -1);
            }
            case "1100011" -> {
                String nm;
                switch (cmd.f3) {
                    case "000" -> nm = "beq";
                    case "001" -> nm = "bne";
                    case "100" -> nm = "blt";
                    case "101" -> nm = "bge";
                    case "110" -> nm = "bltu";
                    case "111" -> nm = "bgeu";
                    default -> {
                        return new AnswStruct("unknown", false, -1);
                    }

                }
                int temp_imm = Integer.parseInt(cmd.returnBits(31, 31) + cmd.returnBits(7, 7) + cmd.returnBits(25, 30) + cmd.returnBits(8, 11), 2) * 2 -
                        4096 * 2 * Integer.parseInt(cmd.returnBits(31, 31));
                int rs2 = Integer.parseInt(cmd.returnBits(20, 24), 2);
                return new AnswStruct(nm + " " + a.getReg(rs1) + ", " + a.getReg(rs2), true, temp_imm);
            }
            case "0000011" -> {
                String nm = "";
                switch (cmd.f3) {
                    case "000" -> nm = "lb";
                    case "001" -> nm = "lh";
                    case "010" -> nm = "lw";
                    case "100" -> nm = "lbu";
                    case "101" -> nm = "lhu";
                    default -> {
                        return new AnswStruct("unknown", false, -1);
                    }
                }
                return new AnswStruct(nm + " " + a.getReg(rd) + ", " + imm + "(" + a.getReg(cmd.rs1) + ")", false, -1);
            }
            case "0100011" -> {
                String nm = "";
                int temp_imm_1 = -4096 * Integer.parseInt(cmd.returnBits(31, 31)) + Integer.parseInt(cmd.returnBits(25, 31) + cmd.returnBits(7, 11), 2);
                int rs2 = Integer.parseInt(cmd.returnBits(20, 24), 2);
                switch (cmd.f3) {
                    case "000" -> nm = "sb";
                    case "001" -> nm = "sh";
                    case "010" -> nm = "sw";
                }
                return new AnswStruct(nm + " " + a.getReg(rs2) + ", " + temp_imm_1 + "(" + a.getReg(rs1) + ")", false, -1);
            }
            case "0010011" -> {
                String nm = "";
                switch (cmd.f3) {
                    case "000" -> nm = "addi";
                    case "010" -> nm = "slti";
                    case "011" -> nm = "sltiu";
                    case "100" -> nm = "xori";
                    case "110" -> nm = "ori";
                    case "111" -> nm = "andi";
                    case "001" -> nm = "slli";
                    case "101" -> {
                        if (cmd.returnBits(30, 30).equals("0")) {
                            nm = "srli";
                            imm = Integer.parseInt(cmd.returnBits(20, 25), 2);
                        } else {
                            nm = "srai";
                            imm = Integer.parseInt(cmd.returnBits(20, 25), 2);
                        }
                    }
                }
                return new AnswStruct(nm + " " + a.getReg(rd) + ", " + a.getReg(rs1) + ", " + imm, false, -1);
            }
            case "0110011" -> {
                String nm = "";
                int rs2 = Integer.parseInt(cmd.returnBits(20, 24), 2);
                if (cmd.returnBits(25, 31).equals("0000001")) {
                    switch (cmd.f3) {
                        case "000" -> nm = "mul";
                        case "001" -> nm = "mulh";
                        case "010" -> nm = "mulhsu";
                        case "011" -> nm = "mulhu";
                        case "100" -> nm = "div";
                        case "101" -> nm = "divu";
                        case "110" -> nm = "rem";
                        case "111" -> nm = "remu";
                    }
                } else {
                    switch (cmd.f3) {
                        case "000" -> {
                            if (cmd.returnBits(30, 30).equals("0")) {
                                nm = "add";
                            } else {
                                nm = "sub";
                            }
                        }
                        case "001" -> nm = "sll";
                        case "010" -> nm = "slt";
                        case "011" -> nm = "sltu";
                        case "100" -> nm = "xor";
                        case "101" -> {
                            if (cmd.returnBits(30, 30).equals("0")) {
                                nm = "srl";
                            } else {
                                nm = "sra";
                            }
                        }
                        case "110" -> nm = "or";
                        case "111" -> nm = "and";
                    }
                }
                return new AnswStruct(nm + " " + a.getReg(rd) + ", " + a.getReg(rs1) + ", " + a.getReg(rs2), false, -1);
            }
            case "1110011" -> {
                String nm;
                int csr = Integer.parseInt(cmd.returnBits(20, 31), 2);
                switch (cmd.f3) {
                    case "000" -> {
                        if (cmd.returnBits(20, 20).equals("0")) {
                            nm = "ecall";
                        } else {
                            nm = "ebreak";
                        }
                        return new AnswStruct(nm, false, -1);
                    }
                    case "001" -> {
                        return new AnswStruct("csrrw " + a.getReg(rd) + ", " + a.getRegCsr(csr) + ", " + a.getReg(rs1), false, -1);
                    }
                    case "010" -> {
                        return new AnswStruct("csrrs " + a.getReg(rd) + ", " + a.getRegCsr(csr) + ", " + a.getReg(rs1), false, -1);
                    }
                    case "011" -> {
                        return new AnswStruct("csrrc " + a.getReg(rd) + ", " + a.getRegCsr(csr) + ", " + a.getReg(rs1), false, -1);
                    }
                    case "101" -> {
                        return new AnswStruct("csrrwi " + a.getReg(rd) + ", " + a.getRegCsr(csr) + ", " + rs1, false, -1);
                    }
                    case "110" -> {
                        return new AnswStruct("csrrsi " + a.getReg(rd) + ", " + a.getRegCsr(csr) + ", " + rs1, false, -1);
                    }
                    case "111" -> {
                        return new AnswStruct("csrrci " + a.getReg(rd) + ", " + a.getRegCsr(csr) + ", " + rs1, false, -1);
                    }
                    default -> {
                        return new AnswStruct("unknown", false, -1);
                    }
                }
            }
            default -> {
                return new AnswStruct("unknown", false, -1);
            }
        }
    }

    public AnswStructRemade parse_command(int start, DizAsmBuilder builder) {
        long first_byte = builder.returnBytes(start, 1);
        long first_bits = (first_byte % 2) + (first_byte / 2) % 2;
        if (first_bits == 2) {
            CommInter command = builder.returnComm(builder.returnBytes(start, 4), 32);
            return new AnswStructRemade(output_command32((Comm32) command), 4);
        } else {
            CommInter command = builder.returnComm(builder.returnBytes(start, 4), 16);
            return new AnswStructRemade(output_command16((Comm16) command), 2);
        }
    }
}

class AnswStructRemade {
    public AnswStruct body;
    public int bit;

    public AnswStructRemade(AnswStruct b, int bb) {
        this.body = b;
        this.bit = bb;
    }
}

class AnswStruct {
    public long number;
    public String result;
    public boolean flag;

    public AnswStruct(String res, boolean fl, long num) {
        this.result = res;
        this.flag = fl;
        this.number = num;
    }
}

class Section {
    public long sName;
    public long sType;
    public long sFlags;
    public long sAddr;
    public long sOff;
    public long sSize;
    public long sLink;
    public long sInf;
    public long sAddral;
    public long sEntersz;

    public Section(long sName, long sType, long sFlags, long sAddr, long sOff, long sSize, long sLink, long sInf, long sAddral, long sEntersz) {
        this.sName = sName;
        this.sType = sType;
        this.sFlags = sFlags;
        this.sAddr = sAddr;
        this.sOff = sOff;
        this.sSize = sSize;
        this.sLink = sLink;
        this.sInf = sInf;
        this.sAddral = sAddral;
        this.sEntersz = sEntersz;
    }
}




